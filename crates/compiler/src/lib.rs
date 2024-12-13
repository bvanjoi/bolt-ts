mod ast;
mod atoms;
mod bind;
pub mod check;
mod emit;
mod errors;
mod ir;
mod keyword;
pub mod parser;
mod resolve;
mod ty;

use std::borrow::Cow;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use atoms::AtomMap;
use bind::{bind, GlobalSymbols};
use bolt_ts_span::{ModuleArena, ModulePath};
use parser::parse_parallel;
use parser::token::TokenKind;
use resolve::resolve;

type Diag = Box<dyn bolt_ts_errors::miette::Diagnostic + Send + Sync + 'static>;

pub struct Output {
    pub module_arena: ModuleArena,
    pub output: String,
    pub diags: Vec<bolt_ts_errors::Diag>,
}

fn current_exe_dir() -> std::path::PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

pub fn eval_from(m: ModulePath) -> Output {
    let mut module_arena = ModuleArena::new();
    let m = module_arena.new_module(m, false);
    let mut atoms = AtomMap::default();
    // atom init
    if cfg!(debug_assertions) {
        for (idx, (name, _)) in keyword::KEYWORDS.iter().enumerate() {
            let t = unsafe { std::mem::transmute::<u8, TokenKind>(idx as u8) };
            if t == TokenKind::Var {
                assert_eq!(t as u8 + 1, TokenKind::Let as u8);
                assert_eq!(t as u8 + 2, TokenKind::Const as u8);
            }
            assert!(t.is_keyword());
            assert_eq!(format!("{t:?}").to_lowercase(), *name);
        }
    }
    for (atom, id) in keyword::KEYWORDS {
        atoms.insert(*id, Cow::Borrowed(atom));
    }
    for (atom, id) in keyword::IDENTIFIER {
        atoms.insert(*id, Cow::Borrowed(atom))
    }

    // parser
    // TODO: build graph
    let mut p = parser::Parser::new();
    let dir = current_exe_dir();
    for (_, file) in bolt_ts_lib::LIB_ENTIRES {
        module_arena.new_module(ModulePath::Real(dir.join(file)), true);
    }
    let atoms = Arc::new(Mutex::new(atoms));
    let herd = bumpalo_herd::Herd::new();
    for (module_id, result) in
        parse_parallel(atoms.clone(), &herd, module_arena.modules(), &module_arena)
    {
        p.insert(module_id, result);
    }

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
            let root = p.root(module_id);
            let is_global = module_arena.get_module(module_id).global;
            (bind(&atoms, root, module_id), is_global)
        })
        .collect::<Vec<_>>();

    let mut global_symbols = GlobalSymbols::default();
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

    for (m, (state, _)) in module_arena.modules().iter().zip(bind_list.into_iter()) {
        let module_id = m.id;
        let root = p.root(module_id);
        let result = resolve(state, root, &p, &global_symbols);
        binder.insert(module_id, result);
    }

    // type check
    let diags = binder.steal_errors(m.id);
    let ty_arena = bumpalo::Bump::new();
    let mut checker = check::TyChecker::new(&ty_arena, &p, &atoms, &mut binder, &global_symbols);
    checker.check_program(p.root(m.id));

    // emit
    let mut emitter = emit::Emit::new(&checker.atoms);
    let output = emitter.emit(p.root(m.id));

    let diags: Vec<_> = checker
        .diags
        .into_iter()
        .chain(diags)
        .chain(p.steal_errors(m.id))
        .collect();

    Output {
        module_arena,
        diags,
        output,
    }
}

pub fn eval_and_emit(entry: PathBuf) {
    let output = eval_from(ModulePath::Real(entry));
    output
        .diags
        .into_iter()
        .for_each(|diag| diag.emit(&output.module_arena));
}

const RED_ZONE: usize = 100 * 1024; // 100k
const STACK_PER_RECURSION: usize = 1 * 1024 * 1024; // 1MB

fn ensure_sufficient_stack<R, F: FnOnce() -> R>(f: F) -> R {
    stacker::maybe_grow(RED_ZONE, STACK_PER_RECURSION, f)
}
