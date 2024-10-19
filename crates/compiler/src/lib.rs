mod ast;
mod atoms;
mod bind;
pub mod check;
mod ecma_refer;
mod emit;
mod errors;
mod keyword;
pub mod parser;
mod ty;

use std::path::PathBuf;

use atoms::AtomMap;
use bind::Binder;
use rts_span::{ModuleArena, ModulePath};

type Diag = Box<dyn rts_errors::miette::Diagnostic + Send + Sync + 'static>;

pub struct Output {
    pub module_arena: ModuleArena,
    pub output: String,
    pub diags: Vec<rts_errors::Diag>,
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

    // bind
    let mut binder = bind::Binder::new(&p.atoms);
    binder.bind_program(root);
    let Binder {
        scope_id_parent_map,
        node_id_to_scope_id,
        symbols,
        res,
        ..
    } = binder;

    // type check
    let ty_arena = bumpalo::Bump::new();
    let mut c = check::TyChecker::new(
        &ty_arena,
        &p.nodes,
        &p.atoms,
        scope_id_parent_map,
        node_id_to_scope_id,
        symbols,
        res,
    );
    c.check_program(root);

    // emit
    let mut emitter = emit::Emit::new(&c.atoms);
    let output = emitter.emit(root);

    let diags = parse_diags.into_iter().chain(c.diags.into_iter()).collect();
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
