mod ast;
mod atoms;
pub mod check;
mod ecma_refer;
mod emit;
mod errors;
mod keyword;
pub mod parser;
mod ty;

use std::path::PathBuf;

use atoms::AtomMap;
use rts_errors::Diag;
use rts_span::{ModuleArena, ModulePath};

pub struct Output {
    pub module_arena: ModuleArena,
    pub output: String,
    pub diags: Vec<Diag>,
}

pub fn eval_from(m: ModulePath) -> Output {
    let mut module_arena = ModuleArena::new();
    let m = module_arena.new_module(m);
    let input = module_arena.content_map.get(&m.id).unwrap();
    let ast_arena = bumpalo::Bump::new();
    let atoms = AtomMap::default();
    let mut p = parser::Parser::new(&ast_arena, atoms);
    let mut s = parser::ParserState::new(&mut p, input.as_bytes(), m.id);
    let root = s.parse();
    let ty_arena = bumpalo::Bump::new();
    let mut c = check::TyChecker::new(&ty_arena, p.atoms);
    c.check_program(root);

    let mut emitter = emit::Emit::new(&c.atoms);
    let output = emitter.emit(root);

    Output {
        module_arena,
        diags: c.diags,
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
