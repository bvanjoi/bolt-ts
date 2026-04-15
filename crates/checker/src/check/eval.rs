use super::TyChecker;
use super::get_declared_ty::EnumMemberValue;

use bolt_ts_ast as ast;
use bolt_ts_atom::Atom;
use bolt_ts_binder::{Symbol, SymbolFlags, SymbolID};

#[derive(Debug, Clone, Copy)]
pub(crate) enum EvalResult {
    Number(f64),
    Str(Atom),
    Err,
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn eval_template_expr(
        &mut self,
        expr: &'cx ast::TemplateExpr<'cx>,
        location: Option<ast::NodeID>,
    ) -> EvalResult {
        debug_assert!(location.is_none_or(|location| self.p.node(location).is_decl()));
        self.eval_template_expr_worker(expr, location)
    }

    fn eval_template_expr_worker(
        &mut self,
        expr: &'cx ast::TemplateExpr<'cx>,
        location: Option<ast::NodeID>,
    ) -> EvalResult {
        let mut result = self.atoms.get(expr.head.text).to_string();
        // let mut resolved_other_files = false;
        // let mut has_external_references = false;
        for span in expr.spans {
            let span_result = self.eval_expr(span.expr, location);
            match span_result {
                EvalResult::Number(n) => result += &n.to_string(),
                EvalResult::Str(s) => {
                    let s = self.atoms.get(s);
                    result += s;
                }
                EvalResult::Err => return EvalResult::Err,
            }
            // resolved_other_files |= span_result == EvalResult::resolved_other_files;
        }
        EvalResult::Str(self.atoms.atom(&result))
    }

    pub(super) fn eval_expr(
        &mut self,
        expr: &'cx ast::Expr<'cx>,
        location: Option<ast::NodeID>,
    ) -> EvalResult {
        debug_assert!(location.is_none_or(|location| self.p.node(location).is_decl()));
        match expr.kind {
            ast::ExprKind::Ident(n) => self.eval_ident(n, SymbolFlags::VALUE, true, location),
            ast::ExprKind::NumLit(n) => EvalResult::Number(n.val),
            ast::ExprKind::StringLit(n) => EvalResult::Str(n.val),
            ast::ExprKind::Paren(n) => self.eval_expr(n.expr, location),
            ast::ExprKind::Bin(n) => {
                let left = self.eval_expr(n.left, location);
                let right = self.eval_expr(n.right, location);
                match (left, right) {
                    (EvalResult::Number(left), EvalResult::Number(right)) => {
                        let result = match n.op.kind {
                            ast::BinOpKind::Add => left + right,
                            ast::BinOpKind::Sub => left - right,
                            ast::BinOpKind::Mul => left * right,
                            ast::BinOpKind::Div => left / right,
                            ast::BinOpKind::Mod => left % right,
                            ast::BinOpKind::Less => (left < right) as i32 as f64,
                            ast::BinOpKind::LessEq => (left <= right) as i32 as f64,
                            ast::BinOpKind::Shl => ((left as i32) << (right as i32)) as f64,
                            ast::BinOpKind::Great => (left > right) as i32 as f64,
                            ast::BinOpKind::GreatEq => (left >= right) as i32 as f64,
                            ast::BinOpKind::Sar => ((left as i32) >> (right as i32)) as f64,
                            ast::BinOpKind::Shr => ((left as i32) >> (right as i32)) as f64,
                            ast::BinOpKind::BitOr => ((left as i32) | (right as i32)) as f64,
                            ast::BinOpKind::BitAnd => ((left as i32) & (right as i32)) as f64,
                            ast::BinOpKind::BitXor => ((left as i32) ^ (right as i32)) as f64,
                            ast::BinOpKind::LogicalOr => {
                                (left != 0.0 || right != 0.0) as i32 as f64
                            }
                            ast::BinOpKind::LogicalAnd => {
                                (left != 0.0 && right != 0.0) as i32 as f64
                            }
                            ast::BinOpKind::EqEq => (left == right) as i32 as f64,
                            ast::BinOpKind::EqEqEq => (left == right) as i32 as f64,
                            ast::BinOpKind::NEq => (left != right) as i32 as f64,
                            ast::BinOpKind::NEqEq => (left != right) as i32 as f64,
                            ast::BinOpKind::Exp => left.powf(right),
                            ast::BinOpKind::Instanceof => todo!(),
                            ast::BinOpKind::In => todo!(),
                            ast::BinOpKind::Satisfies => todo!(),
                            ast::BinOpKind::Nullish => todo!(),
                            ast::BinOpKind::Comma => unreachable!(),
                        };
                        EvalResult::Number(result)
                    }
                    (EvalResult::Number(_), EvalResult::Str(_)) => todo!(),
                    (EvalResult::Str(_), EvalResult::Number(_)) => todo!(),
                    (EvalResult::Str(_), EvalResult::Str(_)) => todo!(),
                    (EvalResult::Err, _) | (_, EvalResult::Err) => EvalResult::Err,
                }
            }
            ast::ExprKind::PrefixUnary(n) => {
                let v = self.eval_expr(n.expr, location);
                let i = match v {
                    EvalResult::Number(i) => i,
                    EvalResult::Str(_) | EvalResult::Err => return EvalResult::Err,
                };
                match n.op {
                    ast::PrefixUnaryOp::Plus => EvalResult::Number(i),
                    ast::PrefixUnaryOp::Minus => EvalResult::Number(-i),
                    ast::PrefixUnaryOp::PlusPlus => EvalResult::Err,
                    ast::PrefixUnaryOp::MinusMinus => EvalResult::Err,
                    ast::PrefixUnaryOp::Tilde => EvalResult::Err,
                    ast::PrefixUnaryOp::Excl => EvalResult::Err,
                }
            }
            _ => EvalResult::Err,
        }
    }

    fn eval_ident(
        &mut self,
        ident: &'cx ast::Ident,
        meaning: SymbolFlags,
        ignore_errors: bool,
        location: Option<ast::NodeID>,
    ) -> EvalResult {
        let symbol = self.resolve_symbol_by_ident(ident);
        if symbol == Symbol::ERR {
            return EvalResult::Err;
        }
        let s = self.symbol(symbol);
        if s.flags.contains(SymbolFlags::ENUM_MEMBER) {
            return match location {
                Some(location) => self.eval_enum_member(symbol, location),
                None => todo!(),
            };
        }

        EvalResult::Err
    }

    fn eval_enum_member(&mut self, symbol: SymbolID, location: ast::NodeID) -> EvalResult {
        let s = self.binder.symbol(symbol);
        let Some(decl) = s.value_decl else {
            todo!("error handle");
        };
        if decl == location {
            todo!("error handle");
        }
        let value = self.get_enum_member_value(self.p.node(decl).expect_enum_member());
        if self.parent(decl) != self.parent(location) {
            // TODO: external mark
        }
        match value {
            EnumMemberValue::Number(v) => EvalResult::Number(v),
            EnumMemberValue::Str(v) => EvalResult::Str(v),
            EnumMemberValue::Err => EvalResult::Err,
        }
    }
}
