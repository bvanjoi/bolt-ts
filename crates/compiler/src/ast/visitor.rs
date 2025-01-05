pub fn visit_program<'cx>(v: &mut impl Visitor<'cx>, program: &'cx super::Program<'cx>) {
    for stmt in program.stmts {
        v.visit_stmt(stmt);
    }
}

pub fn visit_stmt<'cx>(v: &mut impl Visitor<'cx>, stmt: &'cx super::Stmt) {
    use super::StmtKind::*;
    match stmt.kind {
        Class(node) => v.visit_class_decl(node),
        Import(node) => v.visit_import_decl(node),
        Export(_) => {}
        Empty(_) => (),
        Var(_) => (),
        If(_) => (),
        Return(_) => (),
        Block(_) => (),
        Fn(_) => (),
        Expr(_) => (),
        Interface(_) => (),
        Type(_) => (),
        Namespace(_) => (),
        Throw(_) => (),
        Enum(_) => (),
    }
}

pub fn visit_class_decl<'cx>(v: &mut impl Visitor<'cx>, class: &'cx super::ClassDecl<'cx>) {}

pub fn visit_import_decl<'cx>(v: &mut impl Visitor<'cx>, import_decl: &'cx super::ImportDecl<'cx>) {
}

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
    (visit_program, &'cx super::Program<'cx>),
    (visit_stmt, &'cx super::Stmt<'cx>),
    (visit_import_decl, &'cx super::ImportDecl<'cx>),
    (visit_class_decl, &'cx super::ClassDecl<'cx>),
);
