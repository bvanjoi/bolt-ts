use super::ast;
use super::TyChecker;
use crate::bind::{SymbolID, SymbolName};
use crate::ty;

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct SigFlags: u16 {
        const HAS_REST_PARAMETER  = 1 << 0;
        const HAS_ABSTRACT = 1 << 2;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Sig<'cx> {
    pub flags: SigFlags,
    pub params: &'cx [SymbolID],
    pub min_args_count: usize,
    pub ret_ty: &'cx ty::Ty<'cx>,
}

impl Sig<'_> {
    pub fn has_rest_param(&self) -> bool {
        self.flags.contains(SigFlags::HAS_REST_PARAMETER)
    }
}

enum SigDecl<'cx> {
    FnDecl(&'cx ast::FnDecl<'cx>),
    ClassDecl(&'cx ast::ClassDecl<'cx>),
    FnExpr(&'cx ast::FnExpr<'cx>),
    ArrowFnExpr(&'cx ast::ArrowFnExpr<'cx>),
    ClassCtor(&'cx ast::ClassCtor<'cx>),
}

impl<'cx> SigDecl<'cx> {
    fn params(&self) -> ast::ParamsDecl<'cx> {
        match self {
            SigDecl::FnDecl(f) => f.params,
            SigDecl::ClassDecl(c) => &[],
            SigDecl::FnExpr(f) => f.params,
            SigDecl::ArrowFnExpr(f) => f.params,
            SigDecl::ClassCtor(f) => f.params,
        }
    }

    fn id(&self) -> ast::NodeID {
        match self {
            SigDecl::FnDecl(f) => f.id,
            SigDecl::ClassDecl(c) => c.id,
            SigDecl::FnExpr(f) => f.id,
            SigDecl::ArrowFnExpr(f) => f.id,
            SigDecl::ClassCtor(f) => f.id,
        }
    }

    fn has_rest_param(&self) -> bool {
        self.params()
            .last()
            .map_or(false, |param| param.dotdotdot.is_some())
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_sig_from_decl(&mut self, id: ast::NodeID) -> Sig<'cx> {
        let node = self.p.get(id.module()).nodes().get(id);
        if !self.node_id_to_sig.contains_key(&id) {
            let sig = get_sig_from_decl(self, node);
            let prev = self.node_id_to_sig.insert(id, sig);
            assert!(prev.is_none());
        }
        self.node_id_to_sig.get(&id).copied().unwrap()
    }
}

fn get_sig_from_decl<'cx>(checker: &mut TyChecker<'cx>, node: ast::Node<'cx>) -> Sig<'cx> {
    let decl = match node {
        ast::Node::FnDecl(f) => SigDecl::FnDecl(f),
        ast::Node::FnExpr(f) => SigDecl::FnExpr(f),
        ast::Node::ArrowFnExpr(f) => SigDecl::ArrowFnExpr(f),
        ast::Node::ClassDecl(c) => SigDecl::ClassDecl(c),
        ast::Node::ClassCtor(c) => SigDecl::ClassCtor(c),
        _ => unreachable!("{node:#?}"),
    };
    assert!(!checker.node_id_to_sig.contains_key(&decl.id()));
    let mut flags = SigFlags::empty();
    let mut min_args_count = 0;
    let mut params = Vec::with_capacity(8);
    for (i, param) in decl.params().iter().enumerate() {
        let scope_id = checker.binder.get(decl.id().module()).node_id_to_scope_id[&param.id];
        let symbol = checker.binder.get(decl.id().module()).res
            [&(scope_id, SymbolName::Normal(param.name.name))];
        params.push(symbol);
        let is_opt = param.question.is_some() || param.dotdotdot.is_some();
        if !is_opt {
            min_args_count = params.len();
        } else {
            assert!(
                min_args_count < params.len(),
                "required parameters cannot follow an optional parameter."
            )
        }
    }
    if decl.has_rest_param() {
        flags.insert(SigFlags::HAS_REST_PARAMETER);
    }
    if let SigDecl::ClassDecl(c) = decl {
        if let Some(mods) = c.modifiers {
            if mods
                .list
                .iter()
                .any(|m| m.kind == ast::ModifierKind::Abstract)
            {
                flags.insert(SigFlags::HAS_ABSTRACT);
            }
        }
    }
    let params: &[SymbolID] = checker.alloc(params);
    let ret_ty = match node {
        ast::Node::FnDecl(_) => checker.undefined_ty(),
        ast::Node::FnExpr(_) => checker.undefined_ty(),
        ast::Node::ArrowFnExpr(_) => checker.undefined_ty(),
        ast::Node::ClassDecl(_) => checker.undefined_ty(),
        ast::Node::ClassCtor(c) => {
            let Some(class_id) = checker.p.get(node.id().module()).parent_map().parent(c.id) else {
                unreachable!()
            };
            let symbol = checker.get_symbol_of_decl(class_id);
            checker.get_declared_ty_of_symbol(class_id.module(), symbol)
        }
        _ => unreachable!(),
    };
    Sig {
        flags,
        params,
        min_args_count,
        ret_ty,
    }
}
