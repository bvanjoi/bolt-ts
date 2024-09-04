mod ast;
mod atoms;
pub mod check;
mod index;
pub mod parser;
mod span;
mod ty;

pub fn eval(input: &str) {
    let ast_arena = bumpalo::Bump::new();
    let mut p = parser::Parser::new(&ast_arena);
    let mut s = parser::ParserState::new(&mut p, input);
    s.parse();
    let root = s.parse();
    let ty_arena = bumpalo::Bump::new();
    let mut c = check::TyChecker::new(&ty_arena, &p.atoms);
    c.check_program(root);
}
