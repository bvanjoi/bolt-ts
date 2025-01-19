use super::cycle_check::ResolutionKey;
use super::TyChecker;
use crate::bind::SymbolID;
use crate::ir;
use crate::ty::Ty;

impl<'cx> TyChecker<'cx> {
    pub fn get_type_of_var_like(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(id).get_ty() {
            return ty;
        }

        let node_id = id.decl(self.binder);
        let node = self.p.node(node_id);

        if !self.push_ty_resolution(ResolutionKey::Type(id)) {
            // TODO: error handle
            return self.any_ty();
        }

        let ty = if let Some(decl) = node.as_var_decl() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_param_decl() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_prop_signature() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_class_prop_ele() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_object_prop_member() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else if let Some(decl) = node.as_object_shorthand_member() {
            self.get_widened_ty_for_var_like_decl(decl)
        } else {
            unreachable!("node: {node:#?}")
        };
        self.get_mut_symbol_links(id).set_ty(ty);

        if self.pop_ty_resolution().has_cycle() {
            // TODO: error handle
            return self.any_ty();
        }
        ty
    }

    pub(super) fn get_widened_ty_for_var_like_decl(
        &mut self,
        decl: &impl ir::VarLike<'cx>,
    ) -> &'cx Ty<'cx> {
        let ty = if let Some(decl_ty) = decl.decl_ty() {
            Some(self.get_ty_from_type_node(decl_ty))
        } else if let Some(init) = decl.init() {
            let init_ty = self.check_expr_with_cache(init);
            Some(self.widened_ty_from_init(init_ty))
        } else {
            None
        };
        self.widen_ty_for_var_like_decl(ty, decl)
    }

    fn widen_ty_for_var_like_decl(
        &mut self,
        ty: Option<&'cx Ty<'cx>>,
        decl: &impl ir::VarLike<'cx>,
    ) -> &'cx Ty<'cx> {
        if let Some(ty) = ty {
            return self.get_widened_ty(ty);
        }
        if let Some(decl) = self.p.node(decl.id()).as_param_decl() {
            if decl.dotdotdot.is_some() {
                return self.any_ty();
            }
        }
        self.any_ty()
    }
}
