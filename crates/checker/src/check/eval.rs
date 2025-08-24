use crate::check::{get_declared_ty::EnumMemberValue, symbol_info::SymbolInfo};

use super::TyChecker;
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
            todo!("error handle")
        }
        match value {
            EnumMemberValue::Number(v) => EvalResult::Number(v),
            EnumMemberValue::Str(v) => EvalResult::Str(v),
            EnumMemberValue::Err => EvalResult::Err,
        }
    }
}
