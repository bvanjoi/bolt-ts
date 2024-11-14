use super::relation::RelationKind;
use super::sig::Sig;
use super::sig::SigFlags;
use super::ExpectedArgsCount;
use super::TyChecker;
use crate::bind::SymbolKind;
use crate::{ast, errors, ty};
use rts_span::Span;
use thin_vec::thin_vec;

pub(super) trait CallLikeExpr<'cx>: Copy + std::fmt::Debug {
    fn callee(&self) -> &'cx ast::Expr<'cx>;
    fn args(&self) -> ast::Exprs<'cx>;
    fn span(&self) -> Span;
    fn callee_decls(
        &self,
        checker: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> thin_vec::ThinVec<ast::NodeID>;
    fn params(&self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx>;
}

impl<'cx> CallLikeExpr<'cx> for ast::CallExpr<'cx> {
    fn callee(&self) -> &'cx ast::Expr<'cx> {
        self.expr
    }
    fn args(&self) -> ast::Exprs<'cx> {
        self.args
    }
    fn span(&self) -> Span {
        self.span
    }
    fn callee_decls(
        &self,
        checker: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> thin_vec::ThinVec<ast::NodeID> {
        let Some(f) = ty.kind.as_fn() else {
            // unreachable!()
            return Default::default();
        };
        match &checker.symbols.get(f.symbol).kind {
            SymbolKind::Function { decls } => decls.clone(),
            SymbolKind::FnExpr { decl } => thin_vec![*decl],
            _ => unreachable!(),
        }
    }
    fn params(&self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        let Some(f) = ty.kind.as_fn() else {
            unreachable!()
        };
        &f.params
    }
}

impl<'cx> CallLikeExpr<'cx> for ast::NewExpr<'cx> {
    fn callee(&self) -> &'cx ast::Expr<'cx> {
        self.expr
    }
    fn args(&self) -> ast::Exprs<'cx> {
        self.args.unwrap_or_default()
    }
    fn span(&self) -> Span {
        self.span
    }
    fn callee_decls(
        &self,
        checker: &TyChecker<'cx>,
        ty: &'cx ty::Ty<'cx>,
    ) -> thin_vec::ThinVec<ast::NodeID> {
        let Some(class) = ty.kind.as_class() else {
            // unreachable!("{ty:#?}");
            return thin_vec![];
        };
        match &checker.symbols.get(class.symbol).kind {
            SymbolKind::Class { decl } => thin_vec![*decl],
            _ => unreachable!(),
        }
    }
    fn params(&self, ty: &'cx ty::Ty<'cx>) -> ty::Tys<'cx> {
        &[]
    }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn check_call_like_expr(
        &mut self,
        expr: &impl CallLikeExpr<'cx>,
    ) -> &'cx ty::Ty<'cx> {
        self.resolve_call_like_expr(expr)
    }

    fn get_ty_at_pos(params: ty::Tys<'cx>, sig: &Sig<'cx>, pos: usize) -> Option<&'cx ty::Ty<'cx>> {
        let param_count = if sig.has_rest_param() {
            sig.params.len() - 1
        } else {
            sig.params.len()
        };
        if pos < param_count {
            Some(params[pos])
        } else if let Some(array) = params.last().unwrap().kind.as_array() {
            Some(array.ty)
        } else {
            None
        }
    }

    fn resolve_call_like_expr(&mut self, expr: &impl CallLikeExpr<'cx>) -> &'cx ty::Ty<'cx> {
        let ty = self.check_expr(expr.callee());
        let decls = expr.callee_decls(self, ty);
        if decls.is_empty() {
            // TODO: use unreachable
            return ty;
        }
        let mut min_required_params = usize::MAX;
        let mut max_required_params = usize::MIN;
        for decl in &decls {
            let node = self.nodes.get(*decl);
            let sig = self.get_sig_from_decl(node);

            if sig.min_args_count < min_required_params {
                min_required_params = sig.min_args_count;
            }
            max_required_params = if sig.has_rest_param() {
                usize::MAX
            } else {
                usize::max(sig.params.len(), max_required_params)
            }
        }

        // FIXME: overload
        let node = self.nodes.get(decls[0]);
        let sig = self.get_sig_from_decl(node);
        if sig.flags.contains(SigFlags::HAS_ABSTRACT) {
            let error = errors::CannotCreateAnInstanceOfAnAbstractClass {
                span: expr.callee().span(),
            };
            self.push_error(expr.span().module, Box::new(error));
        }

        if min_required_params <= expr.args().len() && expr.args().len() <= max_required_params {
            for (idx, arg) in expr.args().iter().enumerate() {
                let Some(param_ty) = Self::get_ty_at_pos(expr.params(ty), &sig, idx) else {
                    continue;
                };
                let arg_ty = self.check_expr_with_contextual_ty(arg, param_ty);
                self.check_type_related_to_and_optionally_elaborate(
                    arg.span(),
                    arg_ty,
                    param_ty,
                    RelationKind::Assignable,
                    |this, span, source, target| {
                        Box::new(errors::ArgumentOfTyIsNotAssignableToParameterOfTy {
                            span,
                            arg_ty: this.print_ty(source).to_string(),
                            param_ty: this.print_ty(target).to_string(),
                        })
                    },
                )
            }
        } else if min_required_params == max_required_params {
            let x = min_required_params;
            let y = expr.args().len();
            let span = if x < y {
                let lo = expr.args()[y - x].span().lo;
                let hi = expr.args().last().unwrap().span().hi;
                Span::new(lo, hi, expr.span().module)
            } else {
                expr.callee().span()
            };
            let error = errors::ExpectedXArgsButGotY {
                span,
                x: super::ExpectedArgsCount::Count(x),
                y,
            };
            self.push_error(span.module, Box::new(error));
        } else if expr.args().len() > max_required_params {
            let lo = expr.args()[max_required_params].span().lo;
            let hi = expr.args().last().unwrap().span().hi;
            let span = Span::new(lo, hi, expr.span().module);
            let error = errors::ExpectedXArgsButGotY {
                span,
                x: ExpectedArgsCount::Range {
                    lo: min_required_params,
                    hi: max_required_params,
                },
                y: expr.args().len(),
            };
            self.push_error(span.module, Box::new(error));
        } else if expr.args().len() < min_required_params {
            let span = expr.span();
            let error: crate::Diag = if max_required_params == usize::MAX {
                Box::new(errors::ExpectedAtLeastXArgsButGotY {
                    span,
                    x: min_required_params,
                    y: expr.args().len(),
                })
            } else {
                Box::new(errors::ExpectedXArgsButGotY {
                    span,
                    x: ExpectedArgsCount::Range {
                        lo: min_required_params,
                        hi: max_required_params,
                    },
                    y: expr.args().len(),
                })
            };
            self.push_error(span.module, error);
        }
        ty
    }
}
