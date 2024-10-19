use std::usize;

use rts_span::Span;

use super::{relation::RelationKind, sig::Sig, TyChecker};
use crate::bind::{Symbol, SymbolID};
use crate::ty::{self, Ty};
use crate::{ast, errors};

#[derive(Debug, Clone, Copy)]
pub enum ExpectedArgsCount {
    Count(usize),
    Range { lo: usize, hi: usize },
}

impl std::fmt::Display for ExpectedArgsCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedArgsCount::Count(c) => write!(f, "{c}"),
            ExpectedArgsCount::Range { lo, hi } => write!(f, "{lo}-{hi}"),
        }
    }
}

impl<'cx> TyChecker<'cx> {
    fn get_ty_at_pos(
        &self,
        f: &'cx ty::AnonymousTy<'cx>,
        sig: &Sig<'cx>,
        pos: usize,
    ) -> Option<&'cx ty::Ty<'cx>> {
        let param_count = if sig.has_rest_param() {
            sig.params.len() - 1
        } else {
            sig.params.len()
        };
        if pos < param_count {
            Some(f.params[pos])
        } else if let ty::TyKind::Array(array) = f.params.last().unwrap().kind {
            Some(array.ty)
        } else {
            None
        }
    }

    pub(super) fn resolve_call_expr(&mut self, expr: &'cx ast::CallExpr<'cx>) -> &'cx Ty<'cx> {
        let fn_ty = self.check_expr(expr.expr);
        let Some(f) = fn_ty.kind.as_anonymous() else {
            todo!()
        };
        let symbol = self.type_symbol[&fn_ty.id];

        use crate::bind::SymbolKind::*;
        let fs = match &self.symbols.get(symbol).kind {
            Err => todo!(),
            FunctionScopedVar => todo!(),
            BlockScopedVar => todo!(),
            Function(fs) => fs,
            Class => todo!(),
        };
        let fs = fs.clone();

        let mut min_required_params = usize::MAX;
        let mut max_required_params = usize::MIN;
        for f in &fs {
            let node = self.nodes.get(*f);
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

        let node = self.nodes.get(fs[0]);
        let sig = self.get_sig_from_decl(node);

        if min_required_params <= expr.args.len() && expr.args.len() <= max_required_params {
            for (idx, arg) in expr.args.iter().enumerate() {
                let Some(param_ty) = self.get_ty_at_pos(f, &sig, idx) else {
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
                x: ExpectedArgsCount::Count(x),
                y,
            };
            self.push_error(span.module, Box::new(error));
        } else if expr.args.len() > max_required_params {
            let lo = expr.args[max_required_params].span().lo;
            let hi = expr.args.last().unwrap().span().hi;
            let span = Span::new(lo, hi, expr.span.module);
            let error = errors::ExpectedXArgsButGotY {
                span,
                x: ExpectedArgsCount::Range {
                    lo: min_required_params,
                    hi: max_required_params,
                },
                y: expr.args.len(),
            };
            self.push_error(span.module, Box::new(error));
        } else if expr.args.len() < min_required_params {
            let span = expr.span;
            let error: crate::Diag = if max_required_params == usize::MAX {
                Box::new(errors::ExpectedAtLeastXArgsButGotY {
                    span,
                    x: min_required_params,
                    y: expr.args.len(),
                })
            } else {
                Box::new(errors::ExpectedXArgsButGotY {
                    span,
                    x: ExpectedArgsCount::Range {
                        lo: min_required_params,
                        hi: max_required_params,
                    },
                    y: expr.args.len(),
                })
            };
            self.push_error(span.module, error);
        }

        fn_ty
    }

    pub(super) fn resolve_symbol_by_ident(&mut self, ident: &'cx ast::Ident) -> SymbolID {
        if let Some(id) = self.final_res.get(&ident.id) {
            return *id;
        }
        let res = resolve_symbol_by_ident(self, ident);
        self.final_res.insert(ident.id, res);
        res
    }
}

fn resolve_symbol_by_ident(checker: &TyChecker, ident: &ast::Ident) -> SymbolID {
    assert!(!checker.final_res.contains_key(&ident.id));
    let name = ident.name;
    let Some(mut scope_id) = checker.node_id_to_scope_id.get(&ident.id).copied() else {
        return Symbol::ERR;
    };
    let res = loop {
        if let Some(id) = checker.res.get(&(scope_id, name)).copied() {
            break id;
        }
        if let Some(parent) = checker.scope_id_parent_map[&scope_id] {
            scope_id = parent;
        } else {
            break Symbol::ERR;
        }
    };
    res
}
