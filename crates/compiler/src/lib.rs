mod ast;
mod atoms;
mod bind;
pub mod check;
mod emit;
mod ir;
mod keyword;
pub mod parser;
mod resolve;
mod ty;
mod utils;

use std::borrow::Cow;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use atoms::AtomMap;
use bind::{bind, GlobalSymbols};
use bolt_ts_config::NormalizedTsConfig;
use bolt_ts_span::ModuleID;
use bolt_ts_span::{ModuleArena, ModulePath};
use parser::parse_parallel;
use parser::token::keyword_idx_to_token;
use parser::token::TokenKind;
use resolve::resolve;
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

fn build_graph<'cx>(
    module_arena: &mut ModuleArena,
    list: &[ModuleID],
    atoms: Arc<Mutex<AtomMap<'cx>>>,
    herd: &'cx bumpalo_herd::Herd,
    parser: &mut parser::Parser<'cx>,
) {
    for (module_id, result) in parse_parallel(atoms.clone(), herd, list, &module_arena) {
        parser.insert(module_id, result);
    }
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

pub fn eval_from(cwd: PathBuf, tsconfig: NormalizedTsConfig) -> Output {
    let mut entries = vec![];
    let mut module_arena = ModuleArena::new();
    // build entires
    let dir = current_exe_dir();
    for (_, file) in bolt_ts_lib::LIB_ENTIRES {
        let module_id = module_arena.new_module(ModulePath::Real(dir.join(file)), true);
        entries.push(module_id);
    }
    for entry in tsconfig.include() {
        let p = std::path::Path::new(entry);
        let list = if p.is_absolute() {
            if p.is_dir() {
                std::fs::read_dir(p)
                    .unwrap()
                    .filter_map(Result::ok)
                    .map(|entry| entry.path())
                    .collect::<Vec<_>>()
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
        };
        entries.extend(list.into_iter().map(|item| {
            let entry = ModulePath::Real(item.to_path_buf());
            module_arena.new_module(entry, false)
        }));
    }

    // atom init
    if cfg!(debug_assertions) {
        for (idx, (name, _)) in keyword::KEYWORDS.iter().enumerate() {
            let t = keyword_idx_to_token(idx);
            if t == TokenKind::Var {
                assert_eq!(t as u8 + 1, TokenKind::Let as u8);
                assert_eq!(t as u8 + 2, TokenKind::Const as u8);
            }
        }
    }
    let mut atoms = AtomMap::new();
    for (atom, id) in keyword::KEYWORDS {
        atoms.insert(*id, Cow::Borrowed(atom));
    }
    for (atom, id) in keyword::IDENTIFIER {
        atoms.insert(*id, Cow::Borrowed(atom))
    }

    // build graph
    let mut p = parser::Parser::new();
    let atoms = Arc::new(Mutex::new(atoms));
    let herd = bumpalo_herd::Herd::new();
    build_graph(&mut module_arena, &entries, atoms.clone(), &herd, &mut p);

    let diags = p.steal_errors();

    // bind
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();
    let mut binder = bind::Binder::new(&p, &atoms);

    // TODO: par
    let bind_list = module_arena
        .modules()
        .iter()
        .map(|m| {
            let module_id = m.id;
            let is_global = m.global;
            let root = p.root(module_id);
            let bind_result = bind(&atoms, &p, root, module_id);
            assert!(!is_global || bind_result.diags.is_empty());
            (bind_result, is_global)
        })
        .collect::<Vec<_>>();

    let mut global_symbols = GlobalSymbols::new();
    for (state, is_global) in &bind_list {
        if !is_global {
            continue;
        }
        for ((scope_id, name), symbol) in state.res.iter() {
            if !scope_id.is_root() {
                continue;
            }
            global_symbols.insert(*name, *symbol);
        }
    }

    // TODO: par
    for (m, (state, _)) in module_arena.modules().iter().zip(bind_list.into_iter()) {
        let module_id = m.id;
        let root = p.root(module_id);
        let result = resolve(state, root, &p, &global_symbols);
        binder.insert(module_id, result);
    }

    // type check
    let diags: Vec<_> = diags.into_iter().chain(binder.steal_errors()).collect();
    let ty_arena = bumpalo::Bump::new();
    let mut checker = check::TyChecker::new(&ty_arena, &p, &atoms, &mut binder, &global_symbols);
    for item in &entries {
        checker.check_program(p.root(*item));
    }

    // codegen
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

    let diags = diags.into_iter().chain(checker.diags.into_iter()).collect();

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
