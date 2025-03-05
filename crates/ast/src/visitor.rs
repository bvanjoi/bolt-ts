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
        Interface(node) => v.visit_interface_decl(node),
        Export(_) => {}
        Empty(_) => (),
        Var(_) => (),
        If(_) => (),
        Return(_) => (),
        Block(_) => (),
        Fn(_) => (),
        Expr(_) => (),
        Type(_) => (),
        Namespace(_) => (),
        Throw(_) => (),
        Enum(_) => (),
        For(_) => (),
        ForOf(_) => (),
        ForIn(_) => (),
        Break(_) => (),
        Continue(_) => {}
        Try(_) => {}
        While(_) => {}
        Do(_) => {}
        Debugger(_) => {}
    }
}

pub fn visit_class_decl<'cx>(v: &mut impl Visitor<'cx>, class: &'cx super::ClassDecl<'cx>) {
    for ele in class.elems.elems {
        v.visit_class_elem(ele);
    }
}

pub fn visit_interface_decl<'cx>(v: &mut impl Visitor<'cx>, i: &'cx super::InterfaceDecl<'cx>) {
    // for ele in i.members {
    //     v.visit_class_elem(ele);
    // }
}

pub fn visit_import_decl<'cx>(v: &mut impl Visitor<'cx>, import_decl: &'cx super::ImportDecl<'cx>) {
}

pub fn visit_class_elem<'cx>(v: &mut impl Visitor<'cx>, elem: &'cx super::ClassElem<'cx>) {
    use super::ClassEleKind::*;
    if let Method(n) = elem.kind {
        v.visit_class_method_elem(n)
    }
}

pub fn visit_class_method_elem<'cx>(
    v: &mut impl Visitor<'cx>,
    method: &'cx super::ClassMethodElem<'cx>,
) {
}

pub fn visit_ty<'cx>(v: &mut impl Visitor<'cx>, ty: &'cx super::Ty<'cx>) {
    use crate::TyKind::*;
    match ty.kind {
        Refer(n) => visit_refer_ty(v, n),
        Array(n) => visit_array_ty(v, n),
        IndexedAccess(n) => visit_indexed_access_ty(v, n),
        Fn(n) => visit_fn_ty(v, n),
        Ctor(n) => visit_ctor_ty(v, n),
        ObjectLit(n) => visit_object_lit_ty(v, n),
        Lit(n) => visit_lit_ty(v, n),
        NamedTuple(n) => visit_named_tuple_ty(v, n),
        Tuple(n) => visit_tuple_ty(v, n),
        Rest(n) => visit_rest_ty(v, n),
        Cond(n) => visit_cond_ty(v, n),
        Union(n) => visit_union_ty(v, n),
        Intersection(n) => visit_intersection_ty(v, n),
        Typeof(n) => visit_typeof_ty(v, n),
        Mapped(n) => visit_mapped_ty(v, n),
        TyOp(n) => visit_ty_op_ty(v, n),
        Pred(n) => visit_pred_ty(v, n),
        Paren(n) => visit_paren_ty(v, n),
        Infer(n) => visit_infer_ty(v, n),
        Intrinsic(n) => visit_intrinsic_ty(v, n),
        Nullable(n) => visit_nullable_ty(v, n),
        TemplateLit(n) => visit_template_lit_ty(v, n),
    }
}

pub fn visit_refer_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ReferTy<'cx>) {
    // TODO: name
    if let Some(ty_args) = n.ty_args {
        for ty in ty_args.list {
            v.visit_ty(ty);
        }
    }
}
pub fn visit_array_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ArrayTy<'cx>) {
    v.visit_ty(n.ele);
}
pub fn visit_indexed_access_ty<'cx>(
    v: &mut impl Visitor<'cx>,
    n: &'cx super::IndexedAccessTy<'cx>,
) {
    v.visit_ty(n.ty);
    v.visit_ty(n.index_ty);
}
pub fn visit_fn_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::FnTy<'cx>) {
    // if let Some(ty_params) = n.ty_params {
    //     for ty_param in ty_params {
    //         v.visit_ty_param(ty_param);
    //     }
    // }
    // for param in n.params {
    //     v.visit_param(param);
    // }
    v.visit_ty(n.ty);
}
pub fn visit_ctor_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::CtorTy<'cx>) {
    // for param in n.params {
    //     v.visit_param(param);
    // }
    v.visit_ty(n.ty);
}
pub fn visit_object_lit_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ObjectLitTy<'cx>) {
    // for prop in n.members {
    //     v.visit_ty_element(prop);
    // }
}
pub fn visit_lit_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::LitTy) {
    // Literal types have no child nodes to visit
}
pub fn visit_named_tuple_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::NamedTupleTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_tuple_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TupleTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_rest_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::RestTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_cond_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::CondTy<'cx>) {
    v.visit_ty(n.check_ty);
    v.visit_ty(n.extends_ty);
    v.visit_ty(n.true_ty);
    v.visit_ty(n.false_ty);
}
pub fn visit_union_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::UnionTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_intersection_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::IntersectionTy<'cx>) {
    for ty in n.tys {
        v.visit_ty(ty);
    }
}
pub fn visit_typeof_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TypeofTy<'cx>) {
    if let Some(ty_args) = n.ty_args {
        for ty in ty_args.list {
            v.visit_ty(ty);
        }
    }
}
pub fn visit_mapped_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::MappedTy<'cx>) {
    if let Some(ty) = n.ty {
        v.visit_ty(ty);
    }
    if let Some(name_ty) = n.name_ty {
        v.visit_ty(name_ty);
    }
}
pub fn visit_ty_op_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TyOp<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_pred_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::PredTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_paren_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::ParenTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_infer_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::InferTy<'cx>) {
    // Infer types have no child nodes to visit
}
pub fn visit_intrinsic_ty<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::IntrinsicTy) {
    // Intrinsic types have no child nodes to visit
}
pub fn visit_nullable_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::NullableTy<'cx>) {
    v.visit_ty(n.ty);
}
pub fn visit_template_lit_ty<'cx>(v: &mut impl Visitor<'cx>, n: &'cx super::TemplateLitTy<'cx>) {
    for span in n.spans {
        v.visit_ty(span.ty);
    }
}
pub fn visit_ident<'cx>(_: &mut impl Visitor<'cx>, _: &'cx super::Ident) {}

macro_rules! make_visitor {
    ( $( ($visit_node: ident, $ty: ty) ),* $(,)? ) => {
      pub trait Visitor<'cx>: Sized {
        $(
          fn $visit_node(&mut self, node: &'cx $ty) {
            $visit_node(self, node)
          }
        )*
      }
    };
}

make_visitor!(
    (visit_program, super::Program<'cx>),
    (visit_stmt, super::Stmt<'cx>),
    (visit_import_decl, super::ImportDecl<'cx>),
    (visit_interface_decl, super::InterfaceDecl<'cx>),
    (visit_class_decl, super::ClassDecl<'cx>),
    (visit_class_elem, super::ClassElem<'cx>),
    (visit_class_method_elem, super::ClassMethodElem<'cx>),
    (visit_ident, super::Ident),
    (visit_ty, super::Ty<'cx>),
    (visit_refer_ty, super::ReferTy<'cx>),
    (visit_array_ty, super::ArrayTy<'cx>),
    (visit_indexed_access_ty, super::IndexedAccessTy<'cx>),
    (visit_fn_ty, super::FnTy<'cx>),
    (visit_ctor_ty, super::CtorTy<'cx>),
    (visit_object_lit_ty, super::ObjectLitTy<'cx>),
    (visit_lit_ty, super::LitTy),
    (visit_named_tuple_ty, super::NamedTupleTy<'cx>),
    (visit_tuple_ty, super::TupleTy<'cx>),
    (visit_rest_ty, super::RestTy<'cx>),
    (visit_cond_ty, super::CondTy<'cx>),
    (visit_union_ty, super::UnionTy<'cx>),
    (visit_intersection_ty, super::IntersectionTy<'cx>),
    (visit_typeof_ty, super::TypeofTy<'cx>),
    (visit_mapped_ty, super::MappedTy<'cx>),
    (visit_ty_op_ty, super::TyOp<'cx>),
    (visit_pred_ty, super::PredTy<'cx>),
    (visit_paren_ty, super::ParenTy<'cx>),
    (visit_infer_ty, super::InferTy<'cx>),
    (visit_intrinsic_ty, super::IntrinsicTy),
    (visit_nullable_ty, super::NullableTy<'cx>),
    (visit_template_lit_ty, super::TemplateLitTy<'cx>),
);

pub fn visit_node<'cx>(v: &mut impl Visitor<'cx>, node: &super::Node<'cx>) {
    macro_rules! v {
        ($( ($variant: ident, $node: ident ) ),* $(,)?) => {
            match node {
                $(
                    super::Node::$variant(n) => v.$node(n),
                )*
                _ => {}, // TODO: Handle all other variants
            }
        };
    }
    v!((Program, visit_program),);
}
