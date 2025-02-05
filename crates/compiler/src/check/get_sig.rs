use super::ast;
use super::TyChecker;
use crate::bind::SymbolID;
use crate::ty;
use crate::ty::SigID;
use crate::ty::SigKind;
use crate::ty::{Sig, SigFlags};

impl<'cx> TyChecker<'cx> {
    pub(super) fn new_sig(&mut self, sig: Sig<'cx>) -> &'cx Sig<'cx> {
        assert!(sig.id == SigID::dummy(), "TODO: hidden id");
        let sig = sig.with_id(self.sigs.len());
        let s = self.alloc(sig);
        self.sigs.push(s);
        s
    }

    pub(super) fn get_sig_from_decl(&mut self, id: ast::NodeID) -> &'cx Sig<'cx> {
        if let Some(sig) = self.get_node_links(id).get_resolved_sig() {
            return sig;
        }
        let node = self.p.node(id);
        let ty_params = if let Some(ty_params) = node.ty_params() {
            let mut res = vec![];
            self.append_ty_params(&mut res, ty_params);
            let ty_params: ty::Tys<'cx> = self.alloc(res);
            Some(ty_params)
        } else {
            None
        };
        let sig = get_sig_from_decl(self, node, ty_params);
        let sig = self.new_sig(sig);
        self.get_mut_node_links(id).set_resolved_sig(sig);
        sig
    }

    pub(super) fn get_sigs_of_symbol(&mut self, id: SymbolID) -> ty::Sigs<'cx> {
        let s = self.binder.symbol(id);
        let sigs = s
            .expect_fn()
            .decls
            .clone()
            .into_iter()
            .enumerate()
            .flat_map(|(i, decl)| {
                if i > 0 && self.p.node(decl).fn_body().is_some() {
                    None
                } else {
                    Some(self.get_sig_from_decl(decl))
                }
            })
            .collect::<Vec<_>>();
        self.alloc(sigs)
    }

    pub(super) fn get_signatures_of_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SigKind,
    ) -> ty::Sigs<'cx> {
        let ty = self.get_reduced_apparent_ty(ty);
        self.get_signatures_of_structured_type(ty, kind)
    }

    fn get_signatures_of_structured_type(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SigKind,
    ) -> ty::Sigs<'cx> {
        if ty.kind.is_structured() {
            self.resolve_structured_type_members(ty);
            self.signatures_of_type(ty, kind)
        } else {
            self.empty_array()
        }
    }

    pub(super) fn get_sig_of_ty_tag(&mut self, id: ast::NodeID) -> Option<&'cx Sig<'cx>> {
        None
    }

    pub(super) fn get_single_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        kind: SigKind,
        allow_members: bool,
    ) -> Option<&'cx Sig<'cx>> {
        if !ty.kind.is_object() {
            return None;
        }
        self.resolve_structured_type_members(ty);
        if !allow_members
            || (self.properties_of_object_type(ty).is_empty()
                && self.index_infos_of_ty(ty).is_empty())
        {
            let call_sigs = self.signatures_of_structured_type(ty, SigKind::Call);
            let ctor_sigs = self.signatures_of_structured_type(ty, SigKind::Constructor);
            if kind == SigKind::Call && call_sigs.len() == 1 && ctor_sigs.is_empty() {
                Some(call_sigs[0])
            } else if kind == SigKind::Constructor && ctor_sigs.len() == 1 && call_sigs.is_empty() {
                Some(ctor_sigs[0])
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline]
    pub(super) fn get_single_call_sig(&mut self, ty: &'cx ty::Ty<'cx>) -> Option<&'cx Sig<'cx>> {
        self.get_single_sig(ty, SigKind::Call, false)
    }

    pub(super) fn get_single_call_or_ctor_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
    ) -> Option<&'cx Sig<'cx>> {
        self.get_single_call_sig(ty)
            .or_else(|| self.get_single_sig(ty, SigKind::Constructor, false))
    }

    pub(super) fn get_base_sig(&mut self, sig: &'cx Sig<'cx>) -> &'cx Sig<'cx> {
        if let Some(ty_params) = sig.ty_params {
            // TODO: cache
            let ty_eraser = self.create_ty_eraser(ty_params);
            let targets = self.alloc(
                ty_params
                    .iter()
                    .map(|_| {
                        // todo: get_constraint
                        self.any_ty
                    })
                    .collect::<Vec<_>>(),
            );
            let base_constraint_mapper = self.create_ty_mapper(ty_params, targets);
            let base_constraints = ty_params
                .iter()
                .map(|ty| self.instantiate_ty(ty, Some(base_constraint_mapper)))
                .collect::<Vec<_>>();
            let mut base_constraints: ty::Tys<'cx> = self.alloc(base_constraints);
            for _ in 0..ty_params.len() - 1 {
                base_constraints = self.instantiate_tys(base_constraints, base_constraint_mapper)
            }
            base_constraints = self.instantiate_tys(base_constraints, ty_eraser);
            let mapper = self.create_ty_mapper(ty_params, base_constraints);
            self.instantiate_sig(sig, mapper, true)
        } else {
            sig
        }
    }

    fn create_erased_sig(&mut self, sig: &'cx Sig<'cx>, ty_params: ty::Tys<'cx>) -> &'cx Sig<'cx> {
        let mapper = self.create_ty_eraser(ty_params);
        self.instantiate_sig(sig, mapper, true)
    }

    pub(super) fn get_erased_sig(&mut self, sig: &'cx Sig<'cx>) -> &'cx Sig<'cx> {
        if let Some(ty_params) = sig.ty_params {
            // TODO: cache
            self.create_erased_sig(sig, ty_params)
        } else {
            sig
        }
    }

    // pub(super) fn is_resolving_return_type_of_sig(&self, sig: &'cx Sig<'cx>) -> bool {}
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
            || node.is_fn_ty()
            || node.is_ctor_ty(),
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
    for param in params_of_node {
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
    let params: &[SymbolID] = checker.alloc(params);
    let ret = match node {
        ast::Node::FnDecl(decl) => decl.ty.map(|ty| ty.id()),
        ast::Node::FnExpr(_) => None,
        ast::Node::ArrowFnExpr(_) => None,
        ast::Node::ClassCtor(c) => {
            let class_id = checker.p.parent(c.id).unwrap();
            Some(class_id)
        }
        ast::Node::CtorSigDecl(c) => c.ty.map(|ty| ty.id()),
        ast::Node::ClassMethodElem(f) => f.ty.map(|ty| ty.id()),
        ast::Node::MethodSignature(f) => f.ty.map(|ty| ty.id()),
        ast::Node::CallSigDecl(f) => f.ty.map(|ty| ty.id()),
        ast::Node::FnTy(f) => Some(f.ty.id()),
        ast::Node::CtorTy(f) => Some(f.ty.id()),
        _ => unreachable!(),
    };
    Sig {
        flags,
        ty_params,
        params,
        min_args_count,
        ret,
        node_id: Some(node.id()),
        target: None,
        mapper: None,
        id: SigID::dummy(),
        class_decl: None,
    }
}
