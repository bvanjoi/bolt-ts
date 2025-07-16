use crate::r#trait;
use bolt_ts_ast as ast;

use super::{Emit, helper};

trait EmitClassLike<'cx> {
    fn emit_class_like(emitter: &mut Emit<'cx>, class: &impl r#trait::ClassLike<'cx>);
}

struct ES5;
impl<'cx> EmitClassLike<'cx> for ES5 {
    fn emit_class_like(emitter: &mut Emit<'cx>, class: &impl r#trait::ClassLike<'cx>) {
        emitter.content.p("var");
        emitter.content.p_whitespace();
        if let Some(ident) = class.name() {
            emitter.emit_ident(ident);
            emitter.content.p_whitespace();
            emitter.ns_names.insert((emitter.scope, ident.name));
        }
        if let Some(extends) = class.extends() {
            emitter
                .helper_flags
                .insert(helper::EmitHelperFlags::EXTENDS);
        }
    }
}

struct ES6;
impl<'cx> EmitClassLike<'cx> for ES6 {
    fn emit_class_like(emitter: &mut Emit<'cx>, class: &impl r#trait::ClassLike<'cx>) {
        emitter.content.p("class");
        emitter.content.p_whitespace();
        if let Some(ident) = class.name() {
            emitter.emit_ident(ident);
            emitter.content.p_whitespace();
            emitter.ns_names.insert((emitter.scope, ident.name));
        }
        if let Some(extends) = class.extends() {
            emitter.content.p("extends");
            emitter.content.p_whitespace();
            emitter.emit_expr(extends.expr_with_ty_args.expr);
            emitter.content.p_whitespace();
        }
        emitter.emit_block_like(class.elems());
    }
}

impl<'cx> Emit<'cx> {
    pub(super) fn emit_class_like(&mut self, class: &impl r#trait::ClassLike<'cx>) {
        ES6::emit_class_like(self, class);
    }

    pub(super) fn emit_class_ele(&mut self, ele: &ast::ClassElem<'cx>) {
        use bolt_ts_ast::ClassEleKind::*;
        match ele.kind {
            Prop(prop) => self.emit_class_prop(prop),
            Method(method) => self.emit_class_method(method),
            IndexSig(_) => {}
            Ctor(ctor) => self.emit_class_ctor(ctor),
            Getter(g) => {
                self.emit_leading_comments(g.span);
                if let Some(body) = g.body {
                    if let Some(mods) = g.modifiers {
                        if mods.flags.contains(ast::ModifierKind::Static) {
                            self.content.p("static");
                            self.content.p_whitespace();
                        }
                    }
                    self.content.p("get");
                    self.content.p_whitespace();
                    self.emit_prop_name(g.name);
                    self.emit_params(&[]);
                    self.content.p_whitespace();
                    self.emit_block_stmt(body);
                }
            }
            Setter(g) => {
                self.emit_leading_comments(g.span);
                if let Some(body) = g.body {
                    if let Some(mods) = g.modifiers {
                        if mods.flags.contains(ast::ModifierKind::Static) {
                            self.content.p("static");
                            self.content.p_whitespace();
                        }
                    }
                    self.content.p("set");
                    self.content.p_whitespace();
                    self.emit_prop_name(g.name);
                    self.emit_params(g.params);
                    self.content.p_whitespace();
                    self.emit_block_stmt(body);
                }
            }
            StaticBlock(n) => {
                self.emit_leading_comments(n.span);
                self.content.p("static");
                self.content.p_whitespace();
                self.content.p("{");
                self.emit_block_stmt(n.body);
                self.content.p("}");
            }
        }
    }

    fn emit_class_ctor(&mut self, ctor: &'cx ast::ClassCtor<'cx>) {
        self.emit_leading_comments(ctor.span);

        if let Some(body) = ctor.body {
            self.content.p("constructor");
            self.emit_params(ctor.params);
            self.content.p_whitespace();

            let block = body;
            self.content.p_l_brace();
            self.content.indent += self.options.indent;

            let has_block_stmt = !block.stmts.is_empty()
                && ctor.params.iter().any(|param| {
                    param.dotdotdot.is_none()
                        && param
                            .modifiers
                            .is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Public))
                });

            if has_block_stmt {
                self.content.p_newline();
            }

            let last_super_call = block.stmts.iter().rev().position(|stmt| {
                if let ast::StmtKind::Expr(expr) = stmt.kind {
                    if let ast::ExprKind::Call(call) = expr.expr.kind {
                        if let ast::ExprKind::Super(_) = call.expr.kind {
                            return true;
                        }
                    }
                }
                false
            });

            let (prev_stmts, after_stmts) = if let Some(last_super_call) = last_super_call {
                block.stmts.split_at(last_super_call + 1)
            } else {
                let after_stmts: ast::Stmts<'cx> = &[];
                (block.stmts, after_stmts)
            };

            self.emit_list(
                prev_stmts,
                |this, elem| this.emit_stmt(elem),
                |this, _| {
                    this.content.p_newline();
                },
            );

            self.emit_list(
                ctor.params,
                |this, param| {
                    if param.dotdotdot.is_none()
                        && param
                            .modifiers
                            .is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Public))
                    {
                        this.content.p_newline();

                        this.content.p("this");
                        this.content.p_dot();
                        this.emit_binding(param.name);
                        this.content.p_whitespace();
                        this.content.p_eq();
                        this.content.p_whitespace();
                        this.emit_binding(param.name);
                    }
                },
                |this, param| {
                    if param.dotdotdot.is_none()
                        && param
                            .modifiers
                            .is_some_and(|ms| ms.flags.contains(ast::ModifierKind::Public))
                    {
                        this.content.p_newline();
                    }
                },
            );

            self.emit_list(
                after_stmts,
                |this, elem| this.emit_stmt(elem),
                |this, _| {
                    this.content.p_newline();
                },
            );

            if has_block_stmt {
                self.content.p_newline();
            }
            self.content.indent -= self.options.indent;
            self.content.p_r_brace();
        }
    }

    fn emit_class_method(&mut self, method: &'cx ast::ClassMethodElem<'cx>) {
        self.emit_leading_comments(method.span);
        if let Some(mods) = method.modifiers {
            if mods.flags.contains(ast::ModifierKind::Static) {
                self.content.p("static");
                self.content.p_whitespace();
            }
        }
        if let Some(body) = method.body {
            self.emit_prop_name(method.name);
            self.emit_params(method.params);
            self.content.p_whitespace();
            self.emit_block_stmt(body);
        }
    }

    fn emit_class_prop(&mut self, prop: &'cx ast::ClassPropElem<'cx>) {
        self.emit_leading_comments(prop.span);
        if let Some(mods) = prop.modifiers {
            if mods.flags.contains(ast::ModifierKind::Abstract) {
                return;
            }
            if mods.flags.contains(ast::ModifierKind::Static) {
                self.content.p("static");
                self.content.p_whitespace();
            }
        }
        self.emit_prop_name(prop.name);
        if let Some(init) = prop.init {
            self.content.p_whitespace();
            self.content.p_eq();
            self.content.p_whitespace();
            self.emit_expr(init);
        }
    }
}
