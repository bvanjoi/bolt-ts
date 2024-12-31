use super::ast;
use super::ty;
use super::TyChecker;

#[derive(Debug, Clone, Copy, PartialEq)]
enum ContextFlags {
    None = 0,
    Signature = 1 << 0,
    NoConstraints = 1 << 1,
    Completions = 1 << 2,
    SkipBindingPatterns = 1 << 3,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_contextual_ty(
        &mut self,
        id: ast::NodeID,
        flags: ContextFlags,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ctx) = self.find_context_node(id, flags == ContextFlags::None) {
            return Some(ctx.ty);
        }
        let Some(parent) = self.p.parent(id) else {
            unreachable!()
        };
        let parent = self.p.node(parent);
        use ast::Node::*;
        match parent {
            VarDecl(decl) => self.get_contextual_ty_for_var_like_decl(decl.id),
            _ => None,
        }
    }

    fn get_contextual_ty_for_var_like_decl(&mut self, id: ast::NodeID) -> Option<&'cx ty::Ty<'cx>> {
        let node = self.p.node(id);
        use ast::Node::*;
        match node {
            VarDecl(decl) => {
                if let Some(init) = decl.init {
                    if let Some(decl_ty) = decl.ty {
                        return Some(self.get_ty_from_type_node(decl_ty));
                    }
                }
            }
            _ => unreachable!(),
        };

        None
    }

    fn get_apparent_ty_of_contextual_ty(
        &mut self,
        node: ast::NodeID,
        flags: ContextFlags,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let contextual_ty = self.get_contextual_ty(node, flags);
        if let Some(ty) = self.instantiate_contextual_ty(contextual_ty, node, flags) {
            if !(flags != ContextFlags::NoConstraints && ty.kind.is_type_variable()) {
                return Some(ty);
            }
        }
        None
    }

    fn instantiate_contextual_ty(
        &mut self,
        ty: Option<&'cx ty::Ty<'cx>>,
        node: ast::NodeID,
        flags: ContextFlags,
    ) -> Option<&'cx ty::Ty<'cx>> {
        if let Some(ty) = ty {
            if ty.kind.maybe_type_of_kind(|kind| kind.is_instantiable()) {
                todo!()
            }
        }
        ty
    }

    fn is_arity_smaller(&mut self, sig: &'cx ty::Sig<'cx>, id: ast::NodeID) -> bool {
        let mut target_params_count = 0;
        let params = self.p.node(id).params().unwrap_or_default();
        while target_params_count < params.len() {
            let param = params[target_params_count];
            if param.init.is_some() || param.question.is_some() || param.dotdotdot.is_some() {
                break;
            }
            target_params_count += 1;
        }
        self.has_effective_rest_param(sig) && sig.get_param_count(self) < target_params_count
    }

    fn get_contextual_call_sig(
        &mut self,
        ty: &'cx ty::Ty<'cx>,
        id: ast::NodeID,
    ) -> Option<&'cx ty::Sig<'cx>> {
        let sigs = self
            .get_sigs_of_ty(ty, ty::SigKind::Call)
            .iter()
            .filter(|sig| !self.is_arity_smaller(sig, id))
            .collect::<Vec<_>>();
        if sigs.is_empty() {
            None
        } else {
            Some(sigs[0])
        }
    }

    pub(super) fn get_contextual_sig(&mut self, id: ast::NodeID) -> Option<&'cx ty::Sig<'cx>> {
        assert!(!self.p.node(id).is_class_method_ele());
        let Some(ty) = self.get_apparent_ty_of_contextual_ty(id, ContextFlags::Signature) else {
            return None;
        };

        if !ty.kind.is_union() {
            return self.get_contextual_call_sig(ty, id);
        }
        None
    }
}
