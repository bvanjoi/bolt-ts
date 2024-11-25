mod ast;
mod atoms;
mod bind;
pub mod check;
mod emit;
mod errors;
mod keyword;
pub mod parser;
mod ty;

use std::path::PathBuf;

use atoms::AtomMap;
use bind::Binder;
use bolt_ts_span::{ModuleArena, ModulePath};

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
    let m = module_arena.new_module(m);
    let input = module_arena.content_map.get(&m.id).unwrap();
    let atoms = AtomMap::default();

    // parser
    let ast_arena = bumpalo::Bump::new();
    let mut p = parser::Parser::new(&ast_arena, atoms);
    let mut s = parser::ParserState::new(&mut p, input.as_bytes(), m.id);
    let root = s.parse();

    let parse_diags = s.steal_diags();

    let dir = current_exe_dir();
    for (_, file) in bolt_ts_lib::LIB_ENTIRES {
        let p = dir.join(file);
        module_arena.new_module(ModulePath::Real(p));
    }

    // bind
    let mut binder = bind::Binder::new(&p.atoms);
    binder.bind_program(root);
    let Binder {
        scope_id_parent_map,
        node_id_to_scope_id,
        symbols,
        res,
        final_res,
        ..
    } = binder;

    // type check
    let ty_arena = bumpalo::Bump::new();
    let mut c = check::TyChecker::new(
        &ty_arena,
        &p.nodes,
        &p.parent_map,
        &p.atoms,
        &scope_id_parent_map,
        &node_id_to_scope_id,
        &symbols,
        &res,
        final_res,
    );
    c.check_program(root);

    // emit
    let mut emitter = emit::Emit::new(&c.atoms);
    let output = emitter.emit(root);

    let diags: Vec<_> = parse_diags.into_iter().chain(c.diags.into_iter()).collect();
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
