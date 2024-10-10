use rts_span::Span;

use super::{relation::RelationKind, TyChecker};
use crate::{ast, bind::SymbolID, errors, ty::Ty};

impl<'cx> TyChecker<'cx> {
    pub(super) fn resolve_call_expr(&mut self, expr: &'cx ast::CallExpr<'cx>) -> &'cx Ty<'cx> {
        let fn_ty = self.check_expr(expr.expr);
        let Some(f) = fn_ty.kind.as_anonymous() else {
            todo!()
        };
        if f.params.len() == expr.args.len() {
            for (idx, arg) in expr.args.iter().enumerate() {
                let Some(param_ty) = f.params.get(idx) else {
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
        } else {
            let x = f.params.len();
            let y = expr.args.len();
            let span = if x < y {
                let lo = expr.args[y - x].span().lo;
                let hi = expr.args.last().unwrap().span().hi;
                Span::new(lo, hi, expr.span.module)
            } else {
                expr.expr.span()
            };
            let error = errors::ExpectedXArgsButGotY {
                span,
                x: x.to_string(),
                y: y as u8,
            };
            self.push_error(span.module, Box::new(error));
        }

        fn_ty
    }

    pub(super) fn resolve_symbol_by_ident(&mut self, ident: &'cx ast::Ident) -> Option<SymbolID> {
        // dbg!(&self.node_id_to_scope_id);
        // dbg!(ident.id);
        let scope_id = *self.node_id_to_scope_id.get(&ident.id)?;
        let name = ident.name;
        // self.res.keys().for_each(|v| {
        //     dbg!(self.atoms.get(v.1));
        // });
        // dbg!(self.atoms.get(name));
        self.res.get(&(scope_id, name)).copied()
    }
}
