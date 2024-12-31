use super::ast;
use super::TyChecker;
use crate::bind::SymbolID;
use crate::ty;
use crate::ty::SigKind;
use crate::ty::{Sig, SigFlags};

fn type_params<'cx>(checker: &mut TyChecker<'cx>, node: &ast::Node<'cx>) -> Option<ty::Tys<'cx>> {
    if let Some(ty_params) = node.ty_params() {
        let ty_params = ty_params
            .iter()
            .map(|param| {
                let symbol = checker.binder.final_res(param.id);
                checker.get_declared_ty_of_symbol(symbol)
            })
            .collect::<Vec<_>>();
        Some(checker.alloc(ty_params))
    } else {
        None
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_sig_from_decl(&mut self, id: ast::NodeID) -> &'cx Sig<'cx> {
        if let Some(sig) = self.get_node_links(id).get_resolved_sig() {
            return sig;
        }
        let node = self.p.node(id);
        let ty_params = type_params(self, &node);
        let sig = get_sig_from_decl(self, node, ty_params);
        let sig = self.alloc(sig);
        self.get_mut_node_links(id).set_resolved_sig(sig);
        sig
    }

    pub(super) fn get_sigs_of_symbol(&mut self, id: SymbolID) -> &'cx [&'cx Sig<'cx>] {
        let s = if let Some(s) = self.binder.get_transient(id) {
            if let Some(s) = s.origin {
                return self.get_sigs_of_symbol(s);
            } else {
                self.binder.symbol(id)
            }
        } else {
            self.binder.symbol(id)
        };
        let sigs = s
            .expect_fn()
            .decls
            .clone()
            .into_iter()
            .map(|id| self.get_sig_from_decl(id))
            .collect::<Vec<_>>();
        self.alloc(sigs)
    }

    pub(super) fn get_sigs_of_ty(&mut self, ty: &'cx ty::Ty<'cx>, kind: SigKind) -> ty::Sigs<'cx> {
        self.resolve_structured_type_members(ty);
        let sigs = self.signatures_of_type(ty, kind);
        sigs
    }
}

fn get_sig_from_decl<'cx>(
    checker: &TyChecker<'cx>,
    node: ast::Node<'cx>,
    ty_params: Option<ty::Tys<'cx>>,
) -> Sig<'cx> {
    assert!(
        node.is_fn_decl()
            || node.is_fn_expr()
            || node.is_arrow_fn_expr()
            || node.is_class_ctor()
            || node.is_ctor_sig_decl()
            || node.is_class_method_ele()
            || node.is_method_signature()
            || node.is_call_sig_decl()
            || node.is_fn_ty(),
        "node: {node:#?}",
    );
    let params_of_node = node.params().unwrap();
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
            if mods.flags.contains(ast::ModifierKind::Abstract) {
                flags.insert(SigFlags::HAS_ABSTRACT);
            }
        }
    }
    let params: &[SymbolID] = checker.alloc(params);
    let ret = match node {
        ast::Node::FnDecl(decl) => None,
        ast::Node::FnExpr(_) => None,
        ast::Node::ArrowFnExpr(_) => None,
        ast::Node::ClassCtor(c) => {
            let class_id = checker.p.parent(c.id).unwrap();
            Some(class_id)
        }
        ast::Node::CtorSigDecl(c) => c.ty.map(|ty| ty.id()),
        ast::Node::ClassMethodEle(f) => f.ret.map(|ty| ty.id()),
        ast::Node::MethodSignature(f) => f.ret.map(|ty| ty.id()),
        ast::Node::CallSigDecl(f) => f.ty.map(|ty| ty.id()),
        ast::Node::FnTy(f) => Some(f.ret_ty.id()),
        _ => unreachable!(),
    };
    Sig {
        flags,
        ty_params,
        params,
        min_args_count,
        ret,
        node_id: node.id(),
        target: None,
        mapper: None,
    }
}
