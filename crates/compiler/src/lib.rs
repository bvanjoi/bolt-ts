mod ast;
mod bind;
pub mod check;
mod early_resolve;
mod emit;
mod graph;
mod ir;
mod keyword;
mod late_resolve;
pub mod parser;
mod ty;
mod wf;

use std::borrow::Cow;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use self::bind::bind_parallel;
use self::bind::GlobalSymbols;
use self::early_resolve::early_resolve_parallel;
use self::graph::build_graph;
use self::parser::token::keyword_idx_to_token;
use self::parser::token::TokenKind;
use self::wf::well_formed_check_parallel;

use bolt_ts_atom::AtomMap;
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_fs::CachedFileSystem;
use bolt_ts_span::{ModuleArena, ModuleID, ModulePath};

use normalize_path::NormalizePath;
use rustc_hash::FxHashMap;

type Diag = Box<dyn bolt_ts_errors::miette::Diagnostic + Send + Sync + 'static>;

pub struct Output {
    pub cwd: PathBuf,
    pub tsconfig: NormalizedTsConfig,
    pub module_arena: ModuleArena,
    pub output: FxHashMap<ModuleID, String>,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

fn current_exe_dir() -> std::path::PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

pub fn output_files(
    cwd: &std::path::Path,
    tsconfig: &NormalizedTsConfig,
    module_arena: &ModuleArena,
    output: &FxHashMap<ModuleID, String>,
) -> FxHashMap<PathBuf, String> {
    let p = |m: ModuleID| match module_arena.get_path(m) {
        bolt_ts_span::ModulePath::Real(p) => p,
        bolt_ts_span::ModulePath::Virtual => todo!(),
    };
    match tsconfig.compiler_options().out_dir() {
        bolt_ts_config::OutDir::OwnRoot => output
            .iter()
            .map(|(m, content)| (p(*m).with_extension("js"), content.to_string()))
            .collect(),
        bolt_ts_config::OutDir::Custom(dir) => {
            let dir = cwd.join(dir);
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

fn init_atom<'atoms>() -> AtomMap<'atoms> {
    if cfg!(debug_assertions) {
        for idx in 0..keyword::KEYWORDS.len() {
            let t = keyword_idx_to_token(idx);
            if t == TokenKind::Var {
                assert_eq!(t as u8 + 1, TokenKind::Let as u8);
                assert_eq!(t as u8 + 2, TokenKind::Const as u8);
            }
        }
    }
    let mut atoms = AtomMap::new(1024 * 128);
    for (atom, id) in keyword::KEYWORDS {
        atoms.insert(*id, Cow::Borrowed(atom));
    }
    for (atom, id) in keyword::IDENTIFIER {
        atoms.insert(*id, Cow::Borrowed(atom))
    }
    atoms
}

pub fn eval_from(cwd: PathBuf, tsconfig: NormalizedTsConfig) -> Output {
    // ==== atom init ====
    let mut atoms = init_atom();

    // ==== fs init ====
    let mut fs = bolt_ts_fs::LocalFS::new(&mut atoms);

    let mut entries = vec![];
    let mut module_arena = ModuleArena::new();

    // ==== collect entires ====
    let dir = current_exe_dir();
    for (_, file) in bolt_ts_lib::LIB_ENTIRES {
        let module_id =
            module_arena.new_module(ModulePath::Real(dir.join(file)), true, &mut fs, &mut atoms);
        entries.push(module_id);
    }

    let include = tsconfig
        .include()
        .iter()
        .flat_map(|entry| {
            let p = std::path::Path::new(entry);
            if p.is_absolute() {
                if p.is_dir() {
                    fs.read_dir(p, &mut atoms).unwrap().collect::<Vec<_>>()
                } else {
                    vec![p.to_path_buf()]
                }
            } else {
                let p = cwd.join(entry);
                let pattern = p.to_string_lossy();
                glob::glob(&pattern)
                    .unwrap()
                    .filter_map(Result::ok)
                    .map(|entry| entry.to_path_buf())
                    .collect::<Vec<_>>()
            }
        })
        .collect::<Vec<_>>();

    entries.extend(include.into_iter().map(|item| {
        let p = item.normalize();
        let entry = ModulePath::Real(p);
        module_arena.new_module(entry, false, &mut fs, &mut atoms)
    }));

    // ==== build graph ====
    let mut p = parser::Parser::new();
    let atoms = Arc::new(Mutex::new(atoms));
    let herd = bumpalo_herd::Herd::new();
    let mg = build_graph(
        &mut module_arena,
        &entries,
        atoms.clone(),
        &herd,
        &mut p,
        fs,
    );

    let diags = p.steal_errors();

    // ==== bind ====
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();

    let bind_list = bind_parallel(module_arena.modules(), &atoms, &p);

    let mut global_symbols = GlobalSymbols::new();
    for state in bind_list
        .iter()
        .zip(module_arena.modules())
        .filter_map(|(state, m)| m.global.then(|| state))
    {
        for ((scope_id, name), symbol) in state.res.iter() {
            if !scope_id.is_root() {
                continue;
            }
            global_symbols.insert(*name, *symbol);
        }
    }

    // ==== name resolution ====
    let early_resolve_result =
        early_resolve_parallel(module_arena.modules(), &bind_list, &p, &global_symbols);

    let mut binder = bind::Binder::new(&p, &atoms);

    for (m, early_resolve_result, mut state) in module_arena
        .modules()
        .iter()
        .zip(early_resolve_result.into_iter())
        .zip(bind_list)
        .map(|((x, y), z)| (x, y, z))
    {
        state.diags.extend(early_resolve_result.diags);
        if cfg!(debug_assertions) {
            for node_id in state.final_res.keys() {
                assert!(!early_resolve_result.final_res.contains_key(node_id));
            }
        }
        state.final_res.extend(early_resolve_result.final_res);
        let result = late_resolve::ResolveResult {
            diags: state.diags,
            symbols: state.symbols,
            final_res: state.final_res,
            deep_res: FxHashMap::default(),
        };
        binder.insert(m.id, result);
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
    let ty_arena = bumpalo::Bump::new();
    let mut checker = check::TyChecker::new(&ty_arena, &p, &atoms, &mut binder, &global_symbols);
    for item in &entries {
        checker.check_program(p.root(*item));
        checker.check_deferred_nodes(*item);
    }

    // ==== codegen ====
    let output = entries
        .iter()
        .filter_map(|item| {
            if module_arena.get_module(*item).global {
                None
            } else {
                let mut emitter = emit::Emit::new(checker.atoms);
                Some((*item, emitter.emit(p.root(*item))))
            }
        })
        .collect::<FxHashMap<_, _>>();

    let diags = diags.into_iter().chain(checker.diags).collect();

    if cfg!(debug_assertions) {
        // each module should be created once
        let paths = module_arena
            .modules()
            .iter()
            .map(|m| m.id)
            .map(|m| {
                if let ModulePath::Real(p) = module_arena.get_path(m) {
                    assert!(
                        p.is_normalized(),
                        "path should be normalized, but got: {:?}",
                        p
                    );
                    p
                } else {
                    todo!()
                }
            })
            .collect::<Vec<_>>();
        let set = paths.iter().collect::<std::collections::HashSet<_>>();
        assert_eq!(paths.len(), set.len());
    }

    Output {
        cwd,
        tsconfig,
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
