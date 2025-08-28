mod cli;
mod diag;
mod r#trait;
mod wf;

use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use self::cli::get_filenames;
use self::wf::well_formed_check_parallel;

use bolt_ts_ast::TokenKind;
use bolt_ts_ast::keyword;
use bolt_ts_ast::keyword_idx_to_token;
use bolt_ts_atom::AtomIntern;
use bolt_ts_binder::bind_parallel;
use bolt_ts_binder::{Binder, ResolveResult};
use bolt_ts_binder::{BinderResult, MergeGlobalSymbolResult};
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_early_resolve::early_resolve_parallel;
use bolt_ts_fs::{CachedFileSystem, read_file_with_encoding};
use bolt_ts_middle::Diag;
use bolt_ts_optimize::{IrOutput, optimize_and_emit};
use bolt_ts_parser::{ParseResultForGraph, ParsedMap};
use bolt_ts_span::{ModuleArena, ModuleID};
use bolt_ts_utils::path::NormalizePath;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

pub const DEFAULT_TSCONFIG: &str = "tsconfig.json";

pub struct Output {
    pub root: PathBuf,
    pub module_arena: ModuleArena,
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub types_len: usize,

    pub ir: Vec<(ModuleID, IrOutput)>,
    pub output: Vec<(ModuleID, String)>,
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

pub fn init_atom() -> AtomIntern {
    #[cfg(debug_assertions)]
    for idx in 0..keyword::KEYWORDS.len() {
        let t = keyword_idx_to_token(idx);
        if t == TokenKind::Var {
            assert_eq!(t as u8 + 1, TokenKind::Let as u8);
            assert_eq!(t as u8 + 2, TokenKind::Const as u8);
        }
    }
    bolt_ts_ast::keyword::init_atom_map()
}

pub fn eval_from(root: PathBuf, tsconfig: &NormalizedTsConfig) -> Output {
    // ==== atom init ====
    let mut atoms = init_atom();

    // ==== fs init ====
    let fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let exe_dir = current_exe_dir();
    let default_libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| exe_dir.join(filename))
        .collect::<Vec<_>>();
    eval_from_with_fs(root, tsconfig, exe_dir, default_libs, fs, atoms)
}

pub fn eval_from_with_fs(
    root: PathBuf,
    tsconfig: &NormalizedTsConfig,
    default_lib_dir: PathBuf,
    default_libs: Vec<PathBuf>,
    mut fs: impl CachedFileSystem,
    mut atoms: bolt_ts_atom::AtomIntern,
) -> Output {
    bolt_ts_tracing::init_tracing();
    // ==== collect entires ====
    let config_file_specs = cli::ConfigFileSpecs::get_config_file_specs(tsconfig);
    let mut include = get_filenames(&config_file_specs, &root, &mut fs, &mut atoms);
    include.sort();
    include.dedup();

    let cap = (default_libs.len() + include.len()) * 2;
    let mut module_arena = ModuleArena::new(cap * 8);

    let entries_with_read_file = default_libs
        .into_iter()
        .map(|lib| (lib, true))
        .chain(include.into_iter().map(|inc| (inc, false)))
        .collect::<Vec<_>>()
        .into_par_iter()
        .map(|(p, is_default_lib)| {
            debug_assert!(p.is_normalized());
            if cfg!(target_arch = "wasm32") {
                (None, p, is_default_lib)
            } else {
                let Ok(content) = read_file_with_encoding(&p) else {
                    panic!("failed to read file: {p:?}");
                };
                (Some(content), p, is_default_lib)
            }
        })
        .collect::<Vec<_>>();

    let default_lib_filename = bolt_ts_libs::get_default_lib_filename(tsconfig.compiler_options());

    let entries = entries_with_read_file
        .into_iter()
        .filter_map(|(content, p, is_default_lib)| {
            if is_default_lib && p.file_name().unwrap() == "test.d.ts" {
                let content = content.unwrap();
                let computed_atom = atoms.atom(&content);
                let atom = fs.add_file(&p, content, Some(computed_atom), &mut atoms);
                assert_eq!(computed_atom, atom);
                return Some(module_arena.new_module_with_content(p, is_default_lib, atom, &atoms));
            }
            if is_default_lib && p.file_name().unwrap() != default_lib_filename {
                return None;
            }
            let m = if cfg!(target_arch = "wasm32") {
                assert!(content.is_none());
                module_arena.new_module(p, is_default_lib, &mut fs, &mut atoms)
            } else {
                let content = content.unwrap();
                let computed_atom = atoms.atom(&content);
                let atom = fs.add_file(&p, content, Some(computed_atom), &mut atoms);
                assert_eq!(computed_atom, atom);
                module_arena.new_module_with_content(p, is_default_lib, atom, &atoms)
            };
            Some(m)
        })
        .collect::<Vec<_>>();

    // ==== build graph ====
    let mut p = bolt_ts_parser::ParsedMap::new();
    let atoms = Arc::new(Mutex::new(atoms));
    let herd = bolt_ts_arena::bumpalo_herd::Herd::new();
    let mut mg = bolt_ts_module_graph::build_graph(
        &mut module_arena,
        &entries,
        atoms.clone(),
        &default_lib_dir,
        &herd,
        &mut p,
        fs,
    );

    // ==== bind ====
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();

    let (bind_list, mut p) = {
        let (bind_list, p_map): (Vec<BinderResult<'_>>, Vec<ParseResultForGraph<'_>>) =
            bind_parallel(module_arena.modules(), &atoms, p, tsconfig)
                .into_iter()
                .unzip();
        let p = ParsedMap::from_map(p_map);
        (bind_list, p)
    };

    let diags = p
        .steal_errors()
        .into_iter()
        .chain(mg.steal_errors())
        .collect::<Vec<_>>();

    let MergeGlobalSymbolResult {
        mut bind_list,
        merged_symbols,
        global_symbols,
    } = bolt_ts_binder::merge_global_symbol(&p, &atoms, bind_list, &module_arena);

    let (flow_nodes, flow_in_nodes) = bind_list
        .iter_mut()
        .map(|x| {
            (
                std::mem::take(&mut x.flow_nodes),
                std::mem::take(&mut x.flow_in_nodes),
            )
        })
        .collect::<(Vec<_>, Vec<_>)>();

    // ==== name resolution ====
    let early_resolve_result = early_resolve_parallel(
        module_arena.modules(),
        &bind_list,
        &p,
        &global_symbols,
        &merged_symbols,
        &atoms,
    );

    let states = module_arena
        .modules()
        .iter()
        .zip(early_resolve_result)
        .zip(bind_list)
        .map(|((x, y), z)| (x, y, z))
        .map(|(_, early_resolve_result, mut state)| {
            debug_assert!(
                state
                    .final_res
                    .keys()
                    .all(|node_id| !early_resolve_result.final_res.contains_key(node_id))
            );
            state.diags.extend(early_resolve_result.diags);
            state.final_res.extend(early_resolve_result.final_res);
            ResolveResult {
                symbols: state.symbols,
                final_res: state.final_res,
                diags: state.diags,
                locals: state.locals,
                parent_map: state.parent_map,
            }
        })
        .collect::<Vec<_>>();

    let mut binder = Binder::new(states);

    let diags: Vec<_> = diags
        .into_iter()
        .chain(binder.steal_errors())
        .chain(well_formed_check_parallel(
            &p,
            &atoms,
            module_arena.modules(),
            tsconfig.compiler_options(),
            &binder.bind_results,
        ))
        .collect();

    // ==== type check ====
    let ty_arena = bolt_ts_arena::bumpalo::Bump::with_capacity(1024 * 1024);
    let merged_res = bolt_ts_checker::check::merge_module_augmentation_list_for_global(
        &p,
        &atoms,
        binder.bind_results,
        &module_arena,
        global_symbols,
        merged_symbols,
    );
    let mut binder = bolt_ts_binder::Binder::new(merged_res.bind_list);
    let mut merged_symbols = merged_res.merged_symbols;
    let mut global_symbols = merged_res.global_symbols;
    let mut checker = bolt_ts_checker::check::TyChecker::new(
        &ty_arena,
        &p,
        &mg,
        atoms,
        tsconfig.compiler_options(),
        flow_nodes,
        flow_in_nodes,
        &module_arena,
        &mut binder,
        &mut merged_symbols,
        &mut global_symbols,
    );
    for item in &entries {
        let is_default_lib = module_arena.get_module(*item).is_default_lib();
        let prev = checker.diags.len();
        let root = p.root(*item);
        checker.check_program(root);
        checker.check_deferred_nodes(*item);
        if p.get(*item).is_external_or_commonjs_module() {
            checker.check_external_module_exports(root);
        }
        assert!(
            !is_default_lib || prev == checker.diags.len(),
            "default lib({:#?}) has error",
            module_arena.get_path(*item)
        );
    }

    let diags = diags
        .into_iter()
        .chain(std::mem::take(&mut checker.diags))
        .collect::<Vec<_>>();
    let diags = diag::get_merged_diags(diags, &p, &module_arena);

    let types_len = checker.ty_len();

    let output = optimize_and_emit(entries, checker);

    debug_assert!({
        // each module should be created at most once
        let paths = module_arena
            .modules()
            .iter()
            .map(|m| module_arena.get_path(m.id()))
            .collect::<Vec<_>>();
        debug_assert!(paths.iter().all(|p| p.is_normalized()));
        paths.len() == paths.iter().collect::<std::collections::HashSet<_>>().len()
    });

    Output {
        root,
        module_arena,
        diags,
        output: output.files,
        ir: output.ir,
        types_len,
    }
}
