use super::ast;
use super::TyChecker;
use crate::bind::SymbolID;
use crate::ty::{Sig, SigFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_sig_from_decl(&mut self, id: ast::NodeID) -> &'cx Sig<'cx> {
        if let Some(sig) = self.node_id_to_sig.get(&id) {
            return sig;
        }
        let node = self.p.node(id);
        let sig = get_sig_from_decl(self, node);
        let sig = self.alloc(sig);
        let prev = self.node_id_to_sig.insert(id, sig);
        assert!(prev.is_none());
        sig
    }

    pub(super) fn get_sigs_of_symbol(&mut self, id: SymbolID) -> &'cx [&'cx Sig<'cx>] {
        let f = self.binder.symbol(id).expect_fn();
        let sigs = f
            .decls
            .clone()
            .into_iter()
            .map(|id| self.get_sig_from_decl(id))
            .collect::<Vec<_>>();
        self.alloc(sigs)
    }
}

fn get_sig_from_decl<'cx>(checker: &mut TyChecker<'cx>, node: ast::Node<'cx>) -> Sig<'cx> {
    assert!(!checker.node_id_to_sig.contains_key(&node.id()));
    assert!(
        node.is_fn_decl()
            || node.is_fn_expr()
            || node.is_arrow_fn_expr()
            // TODO: remove `node.is_class_decl()`
            || node.is_class_decl()
            || node.is_class_ctor()
            || node.is_ctor_sig_decl()
            || node.is_class_method_ele()
    );
    let ty_params = match node {
        ast::Node::FnDecl(decl) => decl.ty_params,
        ast::Node::FnExpr(expr) => expr.ty_params,
        ast::Node::ArrowFnExpr(expr) => expr.ty_params,
        ast::Node::ClassDecl(c) => c.ty_params,
        ast::Node::ClassCtor(c) => c.ty_params,
        ast::Node::CtorSigDecl(c) => c.ty_params,
        ast::Node::ClassMethodEle(f) => f.ty_params,
        _ => unreachable!(),
    };
    let ty_params = ty_params.map(|params| {
        let params = params
            .iter()
            .map(|param| checker.binder.final_res(param.id))
            .collect::<Vec<_>>();
        let params: &'cx [SymbolID] = checker.alloc(params);
        params
    });
    let params_of_node = match node {
        ast::Node::FnDecl(f) => f.params,
        ast::Node::ClassDecl(c) => &[],
        ast::Node::FnExpr(f) => f.params,
        ast::Node::ArrowFnExpr(f) => f.params,
        ast::Node::ClassCtor(f) => f.params,
        ast::Node::CtorSigDecl(f) => f.params,
        ast::Node::ClassMethodEle(f) => f.params,
        _ => unreachable!(),
    };
    let has_rest_param = params_of_node
        .last()
        .map(|param| param.dotdotdot.is_some())
        .unwrap_or_default();

    let mut flags = SigFlags::empty();
    let mut min_args_count = 0;
    let mut params = Vec::with_capacity(params_of_node.len());
    for (i, param) in params_of_node.iter().enumerate() {
        let symbol = checker.binder.final_res(param.id);
        params.push(symbol);
        let is_opt = param.question.is_some() || param.dotdotdot.is_some() || param.init.is_some();
        if !is_opt {
            min_args_count = params.len();
        } else {
            assert!(
                min_args_count < params.len(),
                "required parameters cannot follow an optional parameter."
            )
        }
    }
    if has_rest_param {
        flags.insert(SigFlags::HAS_REST_PARAMETER);
    }
    if let Some(c) = node.as_class_decl() {
        if let Some(mods) = c.modifiers {
            if mods.flags.contains(ast::ModifierFlags::ABSTRACT) {
                flags.insert(SigFlags::HAS_ABSTRACT);
            }
        }
    }
    let params: &[SymbolID] = checker.alloc(params);
    let ret = match node {
        ast::Node::FnDecl(decl) => None,
        ast::Node::FnExpr(_) => None,
        ast::Node::ArrowFnExpr(_) => None,
        ast::Node::ClassDecl(c) => {
            let class_id = checker.p.parent(c.id).unwrap();
            Some(class_id)
        }
        ast::Node::ClassCtor(c) => {
            let class_id = checker.p.parent(c.id).unwrap();
            Some(class_id)
        }
        ast::Node::CtorSigDecl(c) => c.ty.map(|ty| ty.id()),
        ast::Node::ClassMethodEle(f) => f.ret.map(|ty| ty.id()),
        _ => unreachable!(),
    };
    Sig {
        flags,
        ty_params,
        params,
        min_args_count,
        ret,
        node_id: node.id(),
    }
}
