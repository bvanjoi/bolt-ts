use bolt_ts_span::ModuleID;
use thin_vec::thin_vec;

use super::ty::{self, Ty, TyKind};
use super::{F64Represent, TyChecker};
use crate::ast;
use crate::bind::{Symbol, SymbolID, SymbolKind, SymbolName};
use crate::keyword;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_type_of_symbol(&mut self, module: ModuleID, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
            return ty;
        }
        use crate::bind::SymbolKind::*;
        let ty = match &self.binder.get(module).symbols.get(id).kind {
            Err => return self.error_ty(),
            FunctionScopedVar => return self.undefined_ty(),
            BlockScopedVar => return self.undefined_ty(),
            Class { .. } => {
                let ty = self.get_type_of_class_decl(module, id);
                // TODO: delete
                if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
                    return ty;
                }
                self.get_mut_symbol_links(module, id).set_ty(ty);
                // ---
                ty
            }
            Function { .. } | FnExpr { .. } => self.get_type_of_func_decl(module, id),
            Property { .. } => self.get_type_of_prop(module, id),
            Object { .. } => {
                let ty = self.get_type_of_object(module, id);
                // TODO: delete
                if let Some(ty) = self.get_symbol_links(module, id).get_ty() {
                    return ty;
                }
                self.get_mut_symbol_links(module, id).set_ty(ty);
                // ---
                ty
            }
            BlockContainer { .. } => return self.undefined_ty(),
            Interface { .. } => return self.undefined_ty(),
            Index { .. } => return self.undefined_ty(),
        };
        ty
    }

    fn get_type_of_object(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        self.undefined_ty()
    }

    fn get_base_type_variable_of_class(
        &mut self,
        module: ModuleID,
        symbol: SymbolID,
    ) -> &'cx Ty<'cx> {
        let Some(class_ty) = self.get_declared_ty_of_symbol(module, symbol) else {
            unreachable!()
        };
        let Some(i) = class_ty.kind.as_interface() else {
            unreachable!()
        };

        i.base_ctor_ty.unwrap()
    }

    fn get_type_of_prop(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let decl = match self.binder.get(module).symbols.get(symbol).kind {
            crate::bind::SymbolKind::Property { decl, .. } => decl,
            _ => unreachable!(),
        };
        let ty = match self.p.get(decl.module()).nodes().get(decl) {
            ast::Node::ClassPropEle(prop) => prop
                .ty
                .map(|ty| self.get_ty_from_type_node(ty))
                .unwrap_or(self.undefined_ty()),
            ast::Node::PropSignature(prop) => prop
                .ty
                .map(|ty| self.get_ty_from_type_node(ty))
                .unwrap_or(self.undefined_ty()),
            _ => unreachable!(),
        };
        ty
    }

    fn get_type_of_class_decl(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let base = self.get_base_type_variable_of_class(module, symbol);
        self.create_class_ty(ty::ClassTy { module, symbol })
    }

    fn get_type_of_func_decl(&mut self, module: ModuleID, symbol: SymbolID) -> &'cx Ty<'cx> {
        let params = self.get_sig_of_symbol(module, symbol);
        self.create_fn_ty(ty::FnTy {
            params,
            ret: self.undefined_ty(),
            module,
            symbol,
        })
    }

    fn get_sig_of_symbol(&mut self, module: ModuleID, id: SymbolID) -> &'cx [&'cx Ty<'cx>] {
        use crate::bind::SymbolKind::*;
        let decls = match &self.binder.get(module).symbols.get(id).kind {
            Err => todo!(),
            Function { decls, .. } => decls.clone(),
            FnExpr { decl } => thin_vec![*decl],
            FunctionScopedVar => todo!(),
            BlockScopedVar => todo!(),
            Object { .. } => todo!(),
            Property { .. } => todo!(),
            Class { .. } => todo!(),
            BlockContainer { .. } => todo!(),
            Interface { .. } => todo!(),
            Index { .. } => todo!(),
        };

        for decl in decls {
            let params = match self.p.get(decl.module()).nodes().get(decl) {
                ast::Node::FnDecl(f) => f.params,
                ast::Node::ArrowFnExpr(f) => f.params,
                ast::Node::FnExpr(f) => f.params,
                ast::Node::ClassMethodEle(m) => m.params,
                _ => unreachable!("{:#?}", self.p.get(decl.module()).nodes().get(decl)),
            };
            let params = params
                .iter()
                .map(|param| {
                    param
                        .ty
                        .map(|ty| self.get_ty_from_type_node(ty))
                        .unwrap_or(self.any_ty())
                })
                .collect::<Vec<_>>();
            return self.alloc(params);
        }
        todo!()
    }

    pub(super) fn get_ty_from_type_node(&mut self, ty: &'cx ast::Ty<'cx>) -> &'cx Ty<'cx> {
        // TODO: cache
        use ast::TyKind::*;
        match ty.kind {
            Refer(refer) => {
                if refer.name.name == keyword::IDENT_BOOLEAN {
                    assert!(refer.args.is_none());
                    self.boolean_ty()
                } else if refer.name.name == keyword::IDENT_NUMBER {
                    assert!(refer.args.is_none());
                    self.number_ty()
                } else if refer.name.name == keyword::IDENT_STRING {
                    assert!(refer.args.is_none());
                    self.string_ty()
                } else if refer.name.name == keyword::IDENT_ANY {
                    assert!(refer.args.is_none());
                    self.any_ty()
                } else if refer.name.name == keyword::IDENT_VOID {
                    assert!(refer.args.is_none());
                    self.void_ty()
                } else {
                    self.get_type_from_ty_reference(refer)
                }
            }
            Array(array) => self.get_ty_from_array_node(array),
            Fn(_) => self.undefined_ty(),
            Lit(lit) => {
                if !self
                    .binder
                    .get(lit.id.module())
                    .final_res
                    .contains_key(&lit.id)
                {
                    unreachable!()
                }

                let module = lit.id.module();
                let symbol = self.binder.get(module).final_res[&lit.id];
                let SymbolKind::Object(object) = &self.binder.get(module).symbols.get(symbol).kind
                else {
                    unreachable!()
                };
                let members = self.alloc(object.members.clone());
                let declared_props = self.get_props_from_members(module, members);
                self.create_object_lit_ty(ty::ObjectLitTy {
                    members,
                    declared_props,
                    module: lit.id.module(),
                    symbol,
                })
            }
            ExprWithArg(expr) => self.get_type_from_ty_reference(expr),
            NumLit(num) => self.get_number_literal_type(num.val),
            StringLit(_) => self.undefined_ty(),
            Tuple(_) => self.undefined_ty(),
            Rest(_) => self.undefined_ty(),
            IndexAccess(_) => self.undefined_ty(),
            Cond(_) => self.undefined_ty(),
        }
    }

    fn get_ty_from_array_node(&mut self, node: &'cx ast::ArrayTy<'cx>) -> &'cx Ty<'cx> {
        let ele_ty = self.get_ty_from_type_node(node.ele);
        self.create_array_ty(ty::ArrayTy { ty: ele_ty })
    }

    pub(super) fn get_number_literal_type(&mut self, val: f64) -> &'cx Ty<'cx> {
        let key = F64Represent::new(val);
        if let Some(id) = self.num_lit_tys.get(&key) {
            self.tys[id]
        } else {
            let kind = TyKind::NumberLit(self.alloc(ty::NumberLitTy { val }));
            let ty = self.new_ty(kind);
            self.num_lit_tys.insert(key, ty.id);
            ty
        }
    }

    pub(super) fn get_global_type(&mut self, name: SymbolName) -> &'cx Ty<'cx> {
        let Some((m, s)) = self.global_symbols.get(name) else {
            unreachable!()
        };
        self.get_declared_ty_of_symbol(m, s).unwrap()
    }
}
