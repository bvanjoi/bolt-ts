use rustc_hash::FxHashMap;

use super::ty::{self, Ty, TyKind};
use super::TyChecker;
use crate::ast;
use crate::bind::SymbolID;
use crate::keyword;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_type_of_symbol(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        use crate::bind::SymbolKind::*;
        if let Some(links) = self.symbol_links.get(&id) {
            return links.ty;
        }
        let ty = match &self.symbols.get(id).kind {
            Err => return self.error_ty(),
            FunctionScopedVar => return self.undefined_ty(),
            BlockScopedVar => return self.undefined_ty(),
            Class => self.get_type_of_class_decl(id),
            Function { .. } => self.get_type_of_func_decl(id),
            Object { .. } => return self.undefined_ty(),
            Property => return self.undefined_ty(),
        };
        let prev = self.type_symbol.insert(ty.id, id);
        assert!(prev.is_none());
        ty
    }

    fn get_type_of_class_decl(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        self.create_class_ty(ty::ClassTy {})
    }

    fn get_type_of_func_decl(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        let params = self.get_sig_of_symbol(id);
        self.create_fn_ty(ty::FnTy {
            params,
            ret: self.undefined_ty(),
        })
    }

    fn get_sig_of_symbol(&mut self, id: SymbolID) -> &'cx [&'cx Ty<'cx>] {
        use crate::bind::SymbolKind::*;
        let decls = match &self.symbols.get(id).kind {
            Err => todo!(),
            Function { decls: ids } => ids.clone(),
            FunctionScopedVar => todo!(),
            BlockScopedVar => todo!(),
            Class => todo!(),
            Object { .. } => todo!(),
            Property => todo!(),
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

    pub(super) fn get_ty_from_type_node(&mut self, ty: &'cx ast::Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO: cache
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
            Array(array) => self.get_ty_from_array_node(array),
            Fn(_) => self.undefined_ty(),
            Lit(lit) => {
                let entires = lit.members.iter().filter_map(|member| {
                    match member.kind {
                        ast::ObjectTyMemberKind::Prop(prop) => {
                            let Some(ty) = prop.ty else {
                                // TODO:
                                return None;
                            };
                            let member_ty = self.get_ty_from_type_node(ty);
                            let name = match prop.name.kind {
                                ast::PropNameKind::Ident(ident) => ident.name,
                            };
                            Some((name, member_ty))
                        }
                        ast::ObjectTyMemberKind::Method(_) => {
                            // TODO:
                            return None;
                        }
                    }
                });
                let map = FxHashMap::from_iter(entires);
                let members = self.alloc(map);
                if !self.final_res.contains_key(&lit.id) {
                    unreachable!()
                }
                self.create_object_lit_ty(ty::ObjectLitTy {
                    members,
                    symbol: self.final_res[&lit.id],
                })
            }
        }
    }

    fn get_ty_from_array_node(&mut self, node: &'cx ast::ArrayTy<'cx>) -> &'cx Ty<'cx> {
        let ele_ty = self.get_ty_from_type_node(node.ele);
        self.create_array_ty(ty::ArrayTy { ty: ele_ty })
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
}
