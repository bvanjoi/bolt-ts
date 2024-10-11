use super::ty::{self, Ty, TyKind};
use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;
use crate::keyword;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_type_of_symbol(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        use crate::bind::SymbolKind::*;
        let symbol = &self.symbols.get(id);
        match symbol.kind {
            BlockedScopeVar => self.undefined_ty(),
            Function(_) => self.get_type_of_func_decl(id),
        }
    }

    pub(super) fn get_type_of_func_decl(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        let params = self.get_sig_of_symbol(id);
        let ty = self.alloc(ty::AnonymousTy {
            params,
            ret: self.undefined_ty(),
        });
        self.create_object_ty(ty::ObjectTyKind::Anonymous(ty), id)
    }

    fn get_sig_of_symbol(&mut self, id: SymbolID) -> &'cx [&'cx Ty<'cx>] {
        use crate::bind::SymbolKind::*;
        let decls = match &self.symbols.get(id).kind {
            BlockedScopeVar => todo!(),
            Function(ids) => ids.clone(),
        };

        for decl in decls {
            let ast::Node::FnDecl(f) = self.nodes.get(decl) else {
                continue;
            };
            let params = f
                .params
                .iter()
                .map(|param| self.get_ty_from_type_node(param.ty.unwrap()))
                .collect::<Vec<_>>();
            return self.alloc(params);
        }
        todo!()
    }

    pub(super) fn get_ty_from_type_node(&mut self, ty: &'cx ast::Ty) -> &'cx Ty<'cx> {
        use ast::TyKind::*;
        match ty.kind {
            Ident(ident) => {
                if ident.name == keyword::IDENT_BOOLEAN {
                    self.boolean_ty()
                } else if ident.name == keyword::IDENT_NUMBER {
                    self.number_ty()
                } else if ident.name == keyword::IDENT_STRING {
                    self.string_ty()
                } else if ident.name == keyword::IDENT_ANY {
                    self.any_ty()
                } else if ident.name == keyword::IDENT_VOID {
                    self.void_ty()
                } else {
                    todo!()
                }
            }
            Array(_) => self.undefined_ty(),
            Fn(_) => self.undefined_ty(),
        }
    }

    pub(super) fn get_number_literal_type(&mut self, val: f64) -> &'cx Ty<'cx> {
        let key = unsafe { std::mem::transmute::<f64, u64>(val) };
        let id = self.num_lit_tys.get(&key).copied();
        if let Some(id) = id {
            return self.tys[&id];
        }
        let kind = TyKind::NumberLit(self.alloc(ty::NumberLitTy { val }));
        let ty = self.new_ty(kind);
        self.num_lit_tys.insert(key, ty.id);
        ty
    }

    pub(super) fn get_union_type(&mut self, tys: &'cx [&'cx Ty<'cx>]) -> &'cx Ty<'cx> {
        let union = self.alloc(ty::UnionTy { tys });
        self.new_ty(TyKind::Union(union))
    }
}
