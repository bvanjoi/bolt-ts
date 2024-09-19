mod ast;
mod atoms;
pub mod check;
mod ecma_refer;
mod errors;
mod keyword;
pub mod parser;
mod ty;

use std::path::PathBuf;

use rts_errors::Diag;
use rts_span::{ModuleArena, ModulePath};

pub fn eval_from(m: ModulePath) -> (ModuleArena, Vec<Diag>) {
    let mut arena = ModuleArena::new();
    let m = arena.new_module(m);
    let input = arena.content_map.get(&m.id).unwrap();
    let ast_arena = bumpalo::Bump::new();
    let mut p = parser::Parser::new(&ast_arena);
    let mut s = parser::ParserState::new(&mut p, input, m.id);
    let root = s.parse();
    let ty_arena = bumpalo::Bump::new();
    let mut c = check::TyChecker::new(&ty_arena, p.atoms);
    c.check_program(root);
    (arena, std::mem::take(&mut c.diags))
}

pub fn eval_and_emit(entry: PathBuf) {
    let (arena, diags) = eval_from(ModulePath::Real(entry));
    diags.into_iter().for_each(|diag| diag.emit(&arena));
}
