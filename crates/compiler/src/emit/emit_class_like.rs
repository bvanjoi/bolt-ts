use crate::ast;

use super::emit_block_like::BlockLike;
use super::Emit;

pub(super) trait ClassLike<'cx>: BlockLike<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident>;
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>>;
}

impl<'cx> ClassLike<'cx> for ast::ClassDecl<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident> {
        Some(self.name)
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
}

impl<'cx> ClassLike<'cx> for ast::ClassExpr<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident> {
        self.name
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
}

impl<'cx> Emit<'cx> {
    pub(super) fn emit_class_like(&mut self, class: &impl ClassLike<'cx>) {
        self.content.p("class");
        self.content.p_whitespace();
        if let Some(ident) = class.name() {
            self.emit_ident(ident);
            self.content.p_whitespace();
        }
        if let Some(extends) = class.extends() {
            self.content.p("extends");
            self.content.p_whitespace();
            self.emit_expr(extends.expr);
            self.content.p_whitespace();
        }
        self.emit_block_like(class);
    }

    pub(super) fn emit_class_ele(&mut self, ele: &ast::ClassEle<'cx>) {
        use ast::ClassEleKind::*;
        match ele.kind {
            Prop(prop) => {
                self.emit_class_prop(prop);
            }
            Method(method) => self.emit_class_method(method),
            IndexSig(_) => {}
            Ctor(ctor) => self.emit_class_ctor(ctor),
            Getter(p) => {
                if let Some(body) = p.body {
                    self.content.p("get");
                    self.content.p_whitespace();
                    self.emit_prop_name(&p.name);
                    self.emit_params(&[]);
                    self.content.p_whitespace();
                    self.emit_block_stmt(body);
                }
            }
            Setter(p) => {
                if let Some(body) = p.body {
                    self.content.p("set");
                    self.content.p_whitespace();
                    self.emit_prop_name(&p.name);
                    self.emit_params(p.params);
                    self.content.p_whitespace();
                    self.emit_block_stmt(body);
                }
            }
        }
    }

    fn emit_class_ctor(&mut self, ctor: &'cx ast::ClassCtor<'cx>) {
        if let Some(body) = ctor.body {
            self.content.p("constructor");
            self.emit_params(ctor.params);
            self.content.p_whitespace();
            self.emit_block_stmt(body);
        }
    }

    fn emit_class_method(&mut self, method: &'cx ast::ClassMethodEle<'cx>) {
        if let Some(body) = method.body {
            self.emit_prop_name(&method.name);
            self.emit_params(method.params);
            self.content.p_whitespace();
            self.emit_block_stmt(body);
        }
    }

    fn emit_class_prop(&mut self, prop: &'cx ast::ClassPropEle<'cx>) {
        self.emit_prop_name(&prop.name);
        if let Some(init) = prop.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }
}
