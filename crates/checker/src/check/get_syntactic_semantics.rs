use ast::ExprKind::*;
use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_binder::Symbol;

use super::TyChecker;

bitflags::bitflags! {
  #[derive(Clone, Copy, Debug, PartialEq)]
  pub(super) struct PredicateSemantics: u8 {
    const ALWAYS        = 1 << 0;
    const NEVER         = 1 << 1;
    const SOMETIMES     = Self::ALWAYS.bits() | Self::NEVER.bits();
  }
}

impl<'cx> TyChecker<'cx> {
    pub(super) fn get_syntactic_nullishness_semantics(
        &self,
        node: &'cx ast::Expr<'cx>,
    ) -> PredicateSemantics {
        let node = ast::Expr::skip_outer_expr(node);
        match node.kind {
            Await(_) | Call(_) | TaggedTemplate(_) | EleAccess(_) | New(_) | PropAccess(_)
            | Yield(_) | This(_) => return PredicateSemantics::SOMETIMES,
            Bin(n) => match n.op.kind {
                ast::BinOpKind::LogicalOr | ast::BinOpKind::LogicalAnd => {
                    return PredicateSemantics::SOMETIMES;
                }
                ast::BinOpKind::Comma | ast::BinOpKind::Nullish => {
                    return self.get_syntactic_nullishness_semantics(n.right);
                }
                _ => PredicateSemantics::NEVER,
            },
            Assign(n) => match n.op {
                ast::AssignOp::LogicalOrEq | ast::AssignOp::LogicalAndEq => {
                    return PredicateSemantics::SOMETIMES;
                }
                ast::AssignOp::Eq | ast::AssignOp::NullishEq => {
                    return self.get_syntactic_nullishness_semantics(n.right);
                }
                _ => PredicateSemantics::NEVER,
            },
            Cond(n) => {
                self.get_syntactic_nullishness_semantics(n.when_true)
                    | self.get_syntactic_nullishness_semantics(n.when_false)
            }
            NullLit(_) => PredicateSemantics::ALWAYS,
            Ident(n) => {
                if self.final_res(n.id) == Symbol::ERR {
                    PredicateSemantics::ALWAYS
                } else {
                    PredicateSemantics::SOMETIMES
                }
            }
            _ => PredicateSemantics::NEVER,
        }
    }

    pub(super) fn get_syntactic_truthy_semantics(
        &self,
        node: &'cx ast::Expr<'cx>,
    ) -> PredicateSemantics {
        let node = ast::Expr::skip_outer_expr(node);
        match node.kind {
            NumLit(n) => {
                if n.val == 0. || n.val == 1. {
                    PredicateSemantics::SOMETIMES
                } else {
                    PredicateSemantics::ALWAYS
                }
            }
            ArrayLit(_)
            | ArrowFn(_)
            | BigIntLit(_)
            | Class(_)
            | Fn(_)
            | JsxElem(_)
            | JsxSelfClosingElem(_)
            | ObjectLit(_)
            | RegExpLit(_) => PredicateSemantics::ALWAYS,
            Void(_) | NullLit(_) => PredicateSemantics::NEVER,
            NoSubstitutionTemplateLit(ast::NoSubstitutionTemplateLit { val, .. })
            | StringLit(ast::StringLit { val, .. }) => {
                if *val == keyword::IDENT_EMPTY {
                    PredicateSemantics::NEVER
                } else {
                    PredicateSemantics::ALWAYS
                }
            }
            Cond(n) => {
                self.get_syntactic_truthy_semantics(n.when_true)
                    | self.get_syntactic_truthy_semantics(n.when_false)
            }
            Ident(n) => {
                if self.final_res(n.id) == Symbol::ERR {
                    PredicateSemantics::NEVER
                } else {
                    PredicateSemantics::SOMETIMES
                }
            }
            _ => PredicateSemantics::SOMETIMES,
        }
    }
}
