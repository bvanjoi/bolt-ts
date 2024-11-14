use crate::ast;

use super::Emit;

pub(super) trait ClassLike<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident>;
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>>;
    fn eles(&self) -> ast::ClassEles<'cx>;
}

impl<'cx> ClassLike<'cx> for ast::ClassDecl<'cx> {
    fn name(&self) -> Option<&'cx ast::Ident> {
        Some(self.name)
    }
    fn extends(&self) -> Option<&'cx ast::ClassExtendsClause<'cx>> {
        self.extends
    }
    fn eles(&self) -> ast::ClassEles<'cx> {
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
    fn eles(&self) -> ast::ClassEles<'cx> {
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
        }
        self.content.p_l_brace();
        if !class.eles().is_empty() {
            self.content.p_newline();
        }
        for ele in class.eles() {
            self.emit_class_ele(ele);
        }
        if !class.eles().is_empty() {
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
        }
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
