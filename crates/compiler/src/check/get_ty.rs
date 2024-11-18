use rustc_hash::FxHashMap;
use thin_vec::thin_vec;

use super::ty::{self, Ty, TyKind};
use super::{F64Represent, TyChecker};
use crate::ast;
use crate::bind::{Symbol, SymbolID};
use crate::keyword;

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_type_of_symbol(&mut self, id: SymbolID) -> &'cx Ty<'cx> {
        if let Some(ty) = self.get_symbol_links(id).get_ty() {
            return ty;
        }
        use crate::bind::SymbolKind::*;
        let ty = match &self.symbols.get(id).kind {
            Err => return self.error_ty(),
            FunctionScopedVar => return self.undefined_ty(),
            BlockScopedVar => return self.undefined_ty(),
            Class { .. } => {
                let ty = self.get_type_of_class_decl(id);
                self.get_mut_symbol_links(id).set_ty(ty);
                ty
            }
            Function { .. } | FnExpr { .. } => self.get_type_of_func_decl(id),
            Object { .. } => return self.undefined_ty(),
            Property { .. } => return self.undefined_ty(),
            Method { .. } => return self.undefined_ty(),
        };
        ty
    }

    fn get_effective_base_type_node(&self, id: ast::NodeID) -> Option<&'cx ast::Expr<'cx>> {
        let n = self.nodes.get(id);
        if let ast::Node::ClassDecl(class) = n {
            if let Some(extends) = class.extends {
                return Some(&extends.expr);
            }
        }

        None
    }

    fn get_base_type_variable_of_class(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        let decl = match self.symbols.get(symbol).kind {
            crate::bind::SymbolKind::Class { decl } => decl,
            _ => unreachable!(),
        };
        let Some(base) = self.get_effective_base_type_node(decl) else {
            return self.undefined_ty();
        };
        self.check_expr(base)
    }

    fn get_type_of_class_decl(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        let base = self.get_base_type_variable_of_class(symbol);
        self.create_class_ty(ty::ClassTy { symbol })
    }

    fn get_type_of_func_decl(&mut self, symbol: SymbolID) -> &'cx Ty<'cx> {
        let params = self.get_sig_of_symbol(symbol);
        self.create_fn_ty(ty::FnTy {
            params,
            ret: self.undefined_ty(),
            symbol,
        })
    }

    fn get_sig_of_symbol(&mut self, id: SymbolID) -> &'cx [&'cx Ty<'cx>] {
        use crate::bind::SymbolKind::*;
        let decls = match &self.symbols.get(id).kind {
            Err => todo!(),
            Function { decls, .. } => decls.clone(),
            FnExpr { decl } => thin_vec![*decl],
            FunctionScopedVar => todo!(),
            BlockScopedVar => todo!(),
            Object { .. } => todo!(),
            Property { .. } => todo!(),
            Class { .. } => todo!(),
            Method { .. } => todo!(),
        };

        for decl in decls {
            let params = match self.nodes.get(decl) {
                ast::Node::FnDecl(f) => f.params,
                ast::Node::ArrowFnExpr(f) => f.params,
                ast::Node::FnExpr(f) => f.params,
                _ => unreachable!(),
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
                    // todo!("{}", self.atoms.get(ident.name))
                    self.undefined_ty()
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
            ExprWithArg(expr) => {
                if let ast::ExprKind::Ident(ident) = expr.kind {
                    let id = self.resolve_symbol_by_ident(ident);
                    if id == Symbol::ERR {
                        if let Some(error) = self.check_using_type_as_value(ident) {
                            self.push_error(ident.span.module, error);
                        }
                        return self.error_ty();
                    }
                }
                self.undefined_ty()
            }
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
}
