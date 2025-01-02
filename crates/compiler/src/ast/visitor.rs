use crate::ast;

pub fn visit_program<'cx>(v: &mut impl Visitor<'cx>, program: &'cx ast::Program<'cx>) {
    for stmt in program.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_stmt<'cx>(v: &mut impl Visitor<'cx>, stmt: &'cx ast::Stmt) {
    use ast::StmtKind::*;
    match stmt.kind {
        Class(class) => v.visit_class_decl(class),
        _ => (),
    }
}

pub fn visit_class_decl<'cx>(v: &mut impl Visitor<'cx>, class: &'cx ast::ClassDecl<'cx>) {}

macro_rules! make_visitor {
    ( $( ($visit_node: ident, $ty: ty) ),* $(,)? ) => {
      pub trait Visitor<'cx>: Sized {
        $(
          fn $visit_node(&mut self, node: $ty) {
            $visit_node(self, node)
          }
        )*
      }
    };
}

make_visitor!(
    (visit_program, &'cx ast::Program<'cx>),
    (visit_stmt, &'cx ast::Stmt<'cx>),
    (visit_class_decl, &'cx ast::ClassDecl<'cx>),
);
