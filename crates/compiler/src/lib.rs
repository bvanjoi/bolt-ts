mod bind;
pub mod check;
mod cli;
mod diag;
mod early_resolve;
mod ecma_rules;
mod emit;
mod graph;
mod ir;
mod late_resolve;
pub mod parser;
mod path;
mod ty;
mod wf;

use std::borrow::Cow;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use self::bind::bind_parallel;
use self::bind::{BinderResult, MergeGlobalSymbolResult};
use self::diag::Diag;
use self::early_resolve::early_resolve_parallel;
use self::wf::well_formed_check_parallel;

use bolt_ts_ast::TokenKind;
use bolt_ts_ast::keyword_idx_to_token;

use bolt_ts_ast::keyword;
use bolt_ts_atom::AtomMap;
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_fs::{CachedFileSystem, read_file_with_encoding};
use bolt_ts_span::{ModuleArena, ModuleID, ModulePath};

use cli::get_filenames;
use normalize_path::NormalizePath;
use parser::{ParseResult, Parser};
use rayon::prelude::*;
use rustc_hash::FxHashMap;

pub const DEFAULT_TSCONFIG: &str = "tsconfig.json";

pub struct Output {
    pub root: PathBuf,
    pub module_arena: ModuleArena,
    pub output: Vec<(ModuleID, String)>,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

pub fn current_exe_dir() -> std::path::PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

pub fn output_files(
    root: &std::path::Path,
    tsconfig: &NormalizedTsConfig,
    module_arena: &ModuleArena,
    output: &[(ModuleID, String)],
) -> FxHashMap<PathBuf, String> {
    let p = |m: ModuleID| module_arena.get_path(m);
    match tsconfig.compiler_options().out_dir() {
        bolt_ts_config::OutDir::OwnRoot => output
            .iter()
            .map(|(m, content)| (p(*m).with_extension("js"), content.to_string()))
            .collect(),
        bolt_ts_config::OutDir::Custom(dir) => {
            let dir = root.join(dir);
            output
                .iter()
                .map(|(m, content)| {
                    let file_path = p(*m);
                    let file_name = file_path.file_name().unwrap();
                    (
                        dir.join(file_name).with_extension("js"),
                        content.to_string(),
                    )
                })
                .collect()
        }
    }
}

pub fn init_atom<'atoms>() -> AtomMap<'atoms> {
    if cfg!(test) {
        for idx in 0..keyword::KEYWORDS.len() {
            let t = keyword_idx_to_token(idx);
            if t == TokenKind::Var {
                assert_eq!(t as u8 + 1, TokenKind::Let as u8);
                assert_eq!(t as u8 + 2, TokenKind::Const as u8);
            }
        }
    }
    let mut atoms = AtomMap::new(1024 * 128);
    for (atom, id) in keyword::KEYWORDS.iter().chain(keyword::IDENTIFIER) {
        atoms.insert(*id, Cow::Borrowed(atom));
    }
    atoms
}

pub fn eval_from(root: PathBuf, tsconfig: &NormalizedTsConfig) -> Output {
    // ==== atom init ====
    let mut atoms = init_atom();

    // ==== fs init ====
    let fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let dir = current_exe_dir();
    let libs = bolt_ts_lib::LIB_ENTIRES
        .iter()
        .map(|(_, file)| dir.join(file))
        .collect::<Vec<_>>();
    eval_from_with_fs(root, &tsconfig, libs, fs, atoms)
}

pub fn eval_from_with_fs<'cx>(
    root: PathBuf,
    tsconfig: &'cx NormalizedTsConfig,
    libs: Vec<ModulePath>,
    mut fs: impl CachedFileSystem,
    mut atoms: bolt_ts_atom::AtomMap<'cx>,
) -> Output {
    // ==== collect entires ====
    let config_file_specs = cli::ConfigFileSpecs::get_config_file_specs(tsconfig);
    let mut include = get_filenames(&config_file_specs, &root, &mut fs, &mut atoms);
    include.sort();
    include.dedup();

    let cap = libs.len() + include.len();
    let mut module_arena = ModuleArena::new(cap * 8);

    let entries_with_read_file = libs
        .into_iter()
        .map(|lib| (lib, true))
        .chain(include.into_iter().map(|inc| (inc, false)))
        .collect::<Vec<_>>()
        .into_par_iter()
        .map(|(p, is_global)| {
            debug_assert!(p.is_normalized());
            if cfg!(target_arch = "wasm32") {
                (String::new(), p, is_global)
            } else {
                (read_file_with_encoding(&p).unwrap(), p, is_global)
            }
        })
        .collect::<Vec<_>>();

    let entries = entries_with_read_file
        .into_iter()
        .map(|(content, p, is_global)| {
            if cfg!(target_arch = "wasm32") {
                module_arena.new_module(p, is_global, &mut fs, &mut atoms)
            } else {
                let atom = fs.add_file(&p, content, &mut atoms);
                module_arena.new_module_with_content(p, is_global, atom, &mut atoms)
            }
        })
        .collect::<Vec<_>>();

    // ==== build graph ====
    let mut p = parser::Parser::new();
    let atoms = Arc::new(Mutex::new(atoms));
    let herd = bumpalo_herd::Herd::new();
    let mut mg = graph::build_graph(
        &mut module_arena,
        &entries,
        atoms.clone(),
        &herd,
        &mut p,
        fs,
    );

    let diags = p
        .steal_errors()
        .into_iter()
        .chain(mg.steal_errors())
        .collect::<Vec<_>>();

    // ==== bind ====
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let mut atoms = atoms.into_inner().unwrap();

    let (bind_list, p) = {
        let (bind_list, p_map): (Vec<BinderResult>, Vec<(ParseResult, bind::ParentMap)>) =
            bind_parallel(module_arena.modules(), &atoms, p, &tsconfig)
                .into_iter()
                .unzip();
        let p = Parser::new_with_maps(p_map);
        (bind_list, p)
    };

    let MergeGlobalSymbolResult {
        mut bind_list,
        merged_symbols,
        global_symbols,
    } = bind::merge_global_symbol(&p, bind_list, &module_arena);

    let flow_nodes = bind_list
        .iter_mut()
        .map(|x| std::mem::take(&mut x.flow_nodes))
        .collect::<Vec<_>>();

    // ==== name resolution ====
    let early_resolve_result = early_resolve_parallel(
        module_arena.modules(),
        &bind_list,
        &p,
        &global_symbols,
        &atoms,
    );

    let states = module_arena
        .modules()
        .iter()
        .zip(early_resolve_result)
        .zip(bind_list)
        .map(|((x, y), z)| (x, y, z))
        .map(|(_, early_resolve_result, mut state)| {
            state.diags.extend(early_resolve_result.diags);
            if cfg!(test) {
                for node_id in state.final_res.keys() {
                    assert!(!early_resolve_result.final_res.contains_key(node_id));
                }
            }
            state.final_res.extend(early_resolve_result.final_res);
            state
        })
        .collect::<Vec<_>>();

    let mut binder = bind::Binder::new(&p);
    for (m, res) in late_resolve::late_resolve(
        states,
        module_arena.modules(),
        &mg,
        &p,
        &global_symbols,
        &atoms,
    ) {
        binder.insert(m, res);
    }

    let diags: Vec<_> = diags
        .into_iter()
        .chain(binder.steal_errors())
        .chain(well_formed_check_parallel(
            &p,
            &atoms,
            module_arena.modules(),
        ))
        .collect();

    // ==== type check ====
    let ty_arena = bumpalo::Bump::with_capacity(1024 * 1024);
    let mut checker = check::TyChecker::new(
        &ty_arena,
        &p,
        &mut atoms,
        &binder,
        &global_symbols,
        tsconfig.compiler_options(),
        flow_nodes,
    );
    for item in &entries {
        checker.check_program(p.root(*item));
        checker.check_deferred_nodes(*item);
    }

    // ==== codegen ====
    let output = entries
        .into_par_iter()
        .filter_map(|item| {
            if module_arena.get_module(item).global {
                None
            } else {
                let input_len = module_arena.get_content(item).len();
                let mut emitter = emit::Emit::new(checker.atoms, input_len);
                Some((item, emitter.emit(p.root(item))))
            }
        })
        .collect::<Vec<_>>();

    let diags = diags
        .into_iter()
        .chain(std::mem::take(&mut checker.diags))
        .collect::<Vec<_>>();
    let diags = diag::get_merged_diags(diags, &p, &module_arena);

    if cfg!(test) {
        // each module should be created once
        let paths = module_arena
            .modules()
            .iter()
            .map(|m| {
                let p = module_arena.get_path(m.id);
                debug_assert!(
                    p.is_normalized(),
                    "path should be normalized, but got: {:?}",
                    p
                );
                p
            })
            .collect::<Vec<_>>();
        let set = paths.iter().collect::<std::collections::HashSet<_>>();
        assert_eq!(paths.len(), set.len());
    }

    drop(checker);

    Output {
        root,
        module_arena,
        diags,
        output,
    }
}

const RED_ZONE: usize = 100 * 1024; // 100k
const STACK_PER_RECURSION: usize = 1024 * 1024; // 1MB

fn ensure_sufficient_stack<R, F: FnOnce() -> R>(f: F) -> R {
    stacker::maybe_grow(RED_ZONE, STACK_PER_RECURSION, f)
}
