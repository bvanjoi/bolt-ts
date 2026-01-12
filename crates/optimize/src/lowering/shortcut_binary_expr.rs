use super::LoweringCtx;
use super::ir;
use bolt_ts_ast::{self as ast};
use bolt_ts_ecma_logical::js_double_to_int32;
use bolt_ts_ecma_logical::js_double_to_uint32;

use std::ops::{BitAnd, BitOr, BitXor, Shr};

impl<'checker, 'cx> LoweringCtx<'checker, 'cx> {
    pub(super) fn shortcut_literal_binary_expression(
        &mut self,
        x: ir::Expr,
        y: ir::Expr,
        op: ast::BinOp,
    ) -> Option<ir::NumLitID> {
        if let ir::Expr::NumLit(x) = x
            && let ir::Expr::NumLit(y) = y
        {
            let x = self.nodes.get_num_lit(&x);
            let y = self.nodes.get_num_lit(&y);
            let span = || {
                bolt_ts_span::Span::new(
                    x.span().lo(),
                    y.span().hi(),
                    bolt_ts_span::ModuleID::TRANSIENT,
                )
            };
            match op.kind {
                ast::BinOpKind::Add => {
                    let val = x.val() + y.val();
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, val));
                }
                ast::BinOpKind::Sub => {
                    let val = x.val() - y.val();
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, val));
                }
                ast::BinOpKind::Mul => {
                    let val = x.val() * y.val();
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, val));
                }
                ast::BinOpKind::Div => {
                    let val = x.val() / y.val();
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, val));
                }
                ast::BinOpKind::Mod => {
                    let val = x.val() % y.val();
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, val));
                }
                ast::BinOpKind::BitOr => {
                    let l = js_double_to_int32(x.val());
                    let r = js_double_to_int32(y.val());
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, l.bitor(r) as f64));
                }
                ast::BinOpKind::BitAnd => {
                    let l = js_double_to_int32(x.val());
                    let r = js_double_to_int32(y.val());
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, l.bitand(r) as f64));
                }
                ast::BinOpKind::BitXor => {
                    let l = js_double_to_int32(x.val());
                    let r = js_double_to_int32(y.val());
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, l.bitxor(r) as f64));
                }
                ast::BinOpKind::Shl => {
                    let l = js_double_to_int32(x.val());
                    let r = js_double_to_int32(y.val());
                    let span = span();
                    let val = l.wrapping_shl(r as u32) as f64;
                    return Some(self.nodes.alloc_num_lit(span, val));
                }
                ast::BinOpKind::Sar => {
                    let l = js_double_to_int32(x.val());
                    let r = js_double_to_int32(y.val()) & 0x1F;
                    let val = l.shr(r);
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, val as f64));
                }
                ast::BinOpKind::Shr => {
                    let l = js_double_to_uint32(x.val());
                    let r = js_double_to_int32(y.val());
                    let span = span();
                    let value = l.wrapping_shr(r as u32) as f64;
                    return Some(self.nodes.alloc_num_lit(span, value));
                }
                ast::BinOpKind::Exp => {
                    let val = x.val().powf(y.val());
                    let span = span();
                    return Some(self.nodes.alloc_num_lit(span, val));
                }
                _ => (),
            }
        }

        None
    }
}
