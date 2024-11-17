use crate::ast;

use super::Emit;

pub(super) trait ClassLike<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident>;
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>>;
    fn eles(&self) -> &'cx ast::ClassEles<'cx>;
}

impl<'cx> ClassLike<'cx> for ast::ClassDecl<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident> {
        Some(self.name)
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn eles(&self) -> &'cx ast::ClassEles<'cx> {
        &self.eles
    }
}

impl<'cx> ClassLike<'cx> for ast::ClassExpr<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident> {
        self.name
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn eles(&self) -> &'cx ast::ClassEles<'cx> {
        &self.eles
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
        self.content.p_l_brace();
        if !class.eles().eles.is_empty() {
            self.content.p_newline();
        }
        self.emit_list(
            class.eles().eles,
            |this, ele| this.emit_class_ele(ele),
            |this, ele| {
                if !matches!(ele.kind, ast::ClassEleKind::IndexSig(_)) {
                    this.content.p_newline()
                }
            },
        );
        if !class.eles().eles.is_empty() {
            self.content.p_newline();
        }
        self.content.p_r_brace();
    }

    fn emit_class_ele(&mut self, ele: &'cx ast::ClassEle<'cx>) {
        use ast::ClassEleKind::*;
        match ele.kind {
            Prop(prop) => {
                self.emit_class_prop(prop);
            }
            Method(method) => self.emit_class_method(method),
            IndexSig(_) => {}
            Ctor(ctor) => self.emit_class_ctor(ctor),
        }
    }

    fn emit_class_ctor(&mut self, ctor: &'cx ast::ClassCtor<'cx>) {
        self.content.p("constructor");
        self.emit_params(ctor.params);
        self.content.p_whitespace();
        self.emit_block_stmt(&ctor.body);
    }

    fn emit_class_method(&mut self, method: &'cx ast::ClassMethodEle<'cx>) {
        self.emit_prop_name(&method.name);
        self.emit_params(method.params);
        self.content.p_whitespace();
        self.emit_block_stmt(&method.body);
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
