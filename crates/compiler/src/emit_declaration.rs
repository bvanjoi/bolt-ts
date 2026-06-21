use bolt_ts_ast::{self as ast};
use bolt_ts_ast_visitor::{ControlFlow, Visitor, visit_module_name};
use bolt_ts_checker::{check::TyChecker, emit_resolver::EmitResolver};
use bolt_ts_optimize::Emitter;
use bolt_ts_span::ModuleID;
// use rayon::iter::{IntoParallelIterator, ParallelIterator};

pub fn emit_declarations<'cx, 'a>(
    entries: &Vec<ModuleID>,
    checker: &'a mut TyChecker<'cx>,
) -> Vec<(ModuleID, String)> {
    // let p = &checker.p;
    // let atoms = &checker.atoms;
    // let module_arena = &checker.module_arena;

    // TODO: parallel
    entries
        // .into_par_iter()
        .into_iter()
        .filter_map(|&item| {
            let is_default_lib = checker.module_arena.get_module(item).is_default_lib();
            if is_default_lib {
                None
            } else {
                // let root = p.root(item);
                let output = emit_declaration(item, checker);
                Some((item, output))
            }
        })
        .collect::<Vec<_>>()
}

pub fn emit_declaration<'cx, 'a>(module_id: ModuleID, checker: &'a mut TyChecker<'cx>) -> String {
    let emitter = Emitter::new();
    let mut flags = EmitDeclarationFlags::NEED_DECLARE;
    if checker.p.get(module_id).is_external_or_commonjs_module() {
        flags.remove(EmitDeclarationFlags::NEED_DECLARE);
    }
    let resolver = EmitResolver::new(checker);
    let mut emitter = DeclarationEmitter {
        emitter,
        resolver,
        flags,
    };
    let root = emitter.resolver.program(module_id);
    emitter.visit_program(root);
    emitter.emitter.print().take_content()
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct EmitDeclarationFlags: u8 {
        const STRIP_EXPORT_MODIFIER = 1 << 0;
        const NEED_DECLARE          = 1 << 1;
    }
}
struct DeclarationEmitter<'cx, 'a> {
    emitter: Emitter,
    resolver: EmitResolver<'cx, 'a>,
    flags: EmitDeclarationFlags,
}

impl<'cx, 'a> DeclarationEmitter<'cx, 'a> {
    fn emit_list<T>(
        &mut self,
        list: &[T],
        emit_item: impl Fn(&mut Self, &T),
        emit_sep: impl Fn(&mut Self, &T),
    ) {
        for (idx, item) in list.iter().enumerate() {
            emit_item(self, item);
            if idx != list.len() - 1 {
                emit_sep(self, item)
            }
        }
    }

    fn emit_ret_ty(&mut self, ty: Option<&'cx ast::Ty<'cx>>) {
        match ty {
            Some(ty) => {
                self.visit_ty(ty);
            }
            None => self.emitter.print().p("any"),
        }
    }

    fn emit_type_parameters(&mut self, n: Option<ast::TyParams<'cx>>) {
        if let Some(n) = n {
            debug_assert!(!n.is_empty());
            self.emitter.print().p_less();
            self.emit_list(
                n,
                |this, item| {
                    this.visit_ident(item.name);
                    if let Some(constraint) = item.constraint {
                        this.emitter.print().p_whitespace();
                        this.emitter.print().p("extends");
                        this.emitter.print().p_whitespace();
                        this.visit_ty(constraint);
                    }
                    if let Some(default) = item.default {
                        this.emitter.print().p_whitespace();
                        this.emitter.print().p_eq();
                        this.emitter.print().p_whitespace();
                        this.visit_ty(default);
                    }
                },
                |this, _| {
                    this.emitter.print().p_comma();
                    this.emitter.print().p_whitespace();
                },
            );
            self.emitter.print().p_great();
        }
    }

    fn emit_type_arguments(&mut self, n: Option<&'cx ast::Tys<'cx>>) {
        if let Some(n) = n {
            debug_assert!(!n.list.is_empty());
            self.emitter.print().p_less();
            self.emit_list(
                n.list,
                |this, item| {
                    this.visit_ty(*item);
                },
                |this, _| {
                    this.emitter.print().p_comma();
                    this.emitter.print().p_whitespace();
                },
            );
            self.emitter.print().p_great();
        }
    }

    fn emit_string_literal(&mut self, atom: bolt_ts_atom::Atom) {
        self.emitter.print().p_double_quote();
        self.emitter.emit_atom(self.resolver.atoms(), atom);
        self.emitter.print().p_double_quote();
    }

    fn emit_declare_if_needed(&mut self) {
        if self.flags.contains(EmitDeclarationFlags::NEED_DECLARE) {
            self.emitter.print().p("declare");
            self.emitter.print().p_whitespace();
        }
    }
}

impl<'cx, 'a> Visitor<'cx> for DeclarationEmitter<'cx, 'a> {
    type Result = ();
    fn visit_program(&mut self, node: &'cx ast::Program<'cx>) -> Self::Result {
        for stmt in node.stmts() {
            self.visit_stmt(stmt);
            self.emitter.print().p_newline();
        }
    }

    fn visit_module_decl(&mut self, node: &'cx ast::ModuleDecl<'cx>) -> Self::Result {
        self.emit_declare_if_needed();
        self.emitter.print().p("namespace");
        self.emitter.print().p_whitespace();
        visit_module_name(self, node.name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_brace();
        if let Some(block) = node.block {
            let saved_flags = self.flags;
            self.flags
                .insert(EmitDeclarationFlags::STRIP_EXPORT_MODIFIER);
            self.flags.remove(EmitDeclarationFlags::NEED_DECLARE);
            self.emitter.increment_indent();
            self.emitter.print().p_newline();
            self.emit_list(
                block.stmts,
                |this, stmt| this.visit_stmt(stmt),
                |this, _| this.emitter.print().p_newline(),
            );
            self.emitter.decrement_indent();
            self.emitter.print().p_newline();
            self.flags = saved_flags;
        }
        self.emitter.print().p_r_brace();
    }

    fn visit_ty_param(&mut self, node: &'cx ast::TyParam<'cx>) -> Self::Result {
        self.visit_ident(node.name);
        if let Some(constraint) = node.constraint {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("extends");
            self.emitter.print().p_whitespace();
            self.visit_ty(constraint);
        }
    }

    fn visit_object_lit_ty(&mut self, node: &'cx ast::ObjectLitTy<'cx>) -> Self::Result {
        self.emitter.print().p_l_brace();
        if !node.members.is_empty() {
            self.emitter.increment_indent();
            self.emitter.print().p_newline();
            self.emit_list(
                node.members,
                |this, elem| {
                    use ast::ObjectTyMemberKind::*;
                    match elem.kind {
                        IndexSig(n) => this.visit_index_sig_decl(n),
                        Prop(n) => this.visit_prop_signature(n),
                        Method(n) => this.visit_method_signature(n),
                        CallSig(n) => this.visit_call_sig_decl(n),
                        CtorSig(n) => this.visit_ctor_sig_decl(n),
                        Setter(_n) => todo!(),
                        Getter(_n) => todo!(),
                    }
                },
                |this, _| {
                    this.emitter.print().p_newline();
                },
            );
            self.emitter.decrement_indent();
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn visit_ctor_sig_decl(&mut self, node: &'cx bolt_ts_ast::CtorSigDecl<'cx>) -> Self::Result {
        self.emitter.print().p("new");
        self.emitter.print().p_whitespace();
        self.emit_type_parameters(node.ty_params);
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.emit_ret_ty(node.ty);
        self.emitter.print().p_semi();
    }

    fn visit_call_sig_decl(&mut self, node: &'cx ast::CallSigDecl<'cx>) -> Self::Result {
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();

        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.emit_ret_ty(node.ty);
        self.emitter.print().p_semi();
    }

    fn visit_call_expr(&mut self, _: &'cx ast::CallExpr<'cx>) -> Self::Result {}

    fn visit_class_method_elem(&mut self, node: &'cx ast::ClassMethodElem<'cx>) -> Self::Result {
        if let Some(ms) = node.modifiers {
            if ms.flags.contains(ast::ModifierFlags::STATIC) {
                self.emitter.print().p("static");
                self.emitter.print().p_whitespace();
            }
        }
        self.visit_prop_name(node.name);
        self.emit_type_parameters(node.ty_params);
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        if let Some(ty) = node.ty {
            self.visit_ty(ty);
        } else {
            let ty = self.resolver.ensure_type_for_class_method_element(node);
            let ty_str = self.resolver.print_type(ty);
            self.emitter.print().p(&ty_str);
        }
        self.emitter.print().p_semi();
    }

    fn visit_ctor_ty(&mut self, node: &'cx bolt_ts_ast::CtorTy<'cx>) -> Self::Result {
        self.emitter.print().p("new");
        self.emitter.print().p_whitespace();
        self.emit_type_parameters(node.ty_params);
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
        self.emitter.print().p_arrow_right();
        self.emitter.print().p_whitespace();
        self.visit_ty(node.ty);
        self.emitter.print().p_semi();
    }

    fn visit_class_ctor(&mut self, node: &'cx ast::ClassCtor<'cx>) -> Self::Result {
        self.emitter.print().p("constructor");
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
        if let Some(ty) = node.ret {
            self.emitter.print().p_colon();
            self.emitter.print().p_whitespace();
            self.visit_ty(ty);
        }
        self.emitter.print().p_semi();
    }

    fn visit_class_decl(&mut self, node: &'cx ast::ClassDecl<'cx>) -> Self::Result {
        if let Some(ms) = node.modifiers {
            if ms.flags.contains(ast::ModifierFlags::EXPORT) {
                self.emitter.print().p("export");
                self.emitter.print().p_whitespace();
            }
            if ms.flags.contains(ast::ModifierFlags::DEFAULT) {
                self.emitter.print().p("default");
                self.emitter.print().p_whitespace();
            }
        }
        self.emit_declare_if_needed();
        self.emitter.print().p("class");
        self.emitter.print().p_whitespace();
        if let Some(name) = node.name.map(|name| name.name) {
            self.emitter.emit_atom(self.resolver.atoms(), name);
            self.emitter.print().p_whitespace();
        }
        self.emit_type_parameters(node.ty_params);
        if let Some(extends) = node.extends {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("extends");
            self.emitter.print().p_whitespace();
            self.visit_expr(extends.expr_with_ty_args.expr);
            self.emit_type_arguments(extends.expr_with_ty_args.ty_args);
            self.emitter.print().p_whitespace();
        }
        if let Some(implements) = node.implements {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("implement");
            self.emitter.print().p_whitespace();
            self.emit_list(
                implements.list,
                |this, item| {
                    this.visit_refer_ty(*item);
                },
                |this, _| {
                    this.emitter.print().p_comma();
                    this.emitter.print().p_whitespace();
                },
            );

            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p_l_brace();
        if !node.elems.list.is_empty() {
            self.emitter.increment_indent();
            self.emitter.print().p_newline();
            self.emit_list(
                node.elems.list,
                |this, elem| {
                    use ast::ClassElemKind::*;
                    match elem.kind {
                        Ctor(n) => this.visit_class_ctor(n),
                        Prop(n) => this.visit_class_prop_elem(n),
                        Method(n) => this.visit_class_method_elem(n),
                        IndexSig(n) => this.visit_index_sig_decl(n),
                        Getter(_n) => todo!(),
                        Setter(n) => this.visit_setter_decl(n),
                        StaticBlockDecl(_n) => todo!(),
                        Semi(_) => {}
                    }
                },
                |this, _| {
                    this.emitter.print().p_newline();
                },
            );
            self.emitter.decrement_indent();
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn visit_class_prop_elem(&mut self, node: &'cx ast::ClassPropElem<'cx>) -> Self::Result {
        if let Some(ms) = node.modifiers {
            if ms.flags.contains(ast::ModifierFlags::STATIC) {
                self.emitter.print().p("static");
                self.emitter.print().p_whitespace();
            }
        }
        self.visit_prop_name(node.name);
        if node.question.is_some() {
            self.emitter.print().p_question();
        }
        if let Some(ty) = node.ty {
            self.emitter.print().p_colon();
            self.emitter.print().p_whitespace();
            self.visit_ty(ty);
        }
        self.emitter.print().p_semi();
    }

    fn visit_setter_decl(&mut self, node: &'cx ast::SetterDecl<'cx>) -> Self::Result {
        self.emitter.print().p("set");
        self.emitter.print().p_whitespace();
        self.visit_prop_name(node.name);
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
    }

    fn visit_index_sig_decl(&mut self, node: &'cx ast::IndexSigDecl<'cx>) -> Self::Result {
        self.emitter.print().p("[");
        self.visit_binding(node.key);
        self.emitter.print().p_colon();
        self.visit_ty(node.key_ty);
        self.emitter.print().p("]");
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.visit_ty(node.ty);
        self.emitter.print().p_semi();
    }

    fn visit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) -> Self::Result {
        self.emitter.print().p("interface");
        self.emitter.print().p_whitespace();
        self.emitter
            .emit_atom(self.resolver.atoms(), node.name.name);
        self.emit_type_parameters(node.ty_params);
        if let Some(extends) = node.extends {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("extends");
            self.emitter.print().p_whitespace();
            for item in extends.list {
                self.visit_refer_ty(item);
            }
        }
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_brace();
        if !node.members.is_empty() {
            self.emitter.increment_indent();
            self.emitter.print().p_newline();
            self.emit_list(
                node.members,
                |this, elem| {
                    use ast::ObjectTyMemberKind::*;
                    match elem.kind {
                        IndexSig(_n) => todo!(),
                        Prop(n) => this.visit_prop_signature(n),
                        Method(n) => this.visit_method_signature(n),
                        CallSig(_n) => todo!(),
                        CtorSig(_n) => todo!(),
                        Setter(_n) => todo!(),
                        Getter(_n) => todo!(),
                    }
                },
                |this, _| {
                    this.emitter.print().p_newline();
                },
            );
            self.emitter.decrement_indent();
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn visit_fn_ty(&mut self, node: &'cx ast::FnTy<'cx>) -> Self::Result {
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
        self.emitter.print().p_whitespace();
        self.emitter.print().p_arrow_right();
        self.emitter.print().p_whitespace();
        self.visit_ty(node.ty);
    }

    fn visit_type_alias_decl(&mut self, node: &'cx ast::TypeAliasDecl<'cx>) -> Self::Result {
        self.emitter.print().p("type");
        self.emitter.print().p_whitespace();
        self.emitter
            .emit_atom(self.resolver.atoms(), node.name.name);
        self.emit_type_parameters(node.ty_params);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_eq();
        self.emitter.print().p_whitespace();
        self.visit_ty(node.ty);
        self.emitter.print().p_semi();
    }

    fn visit_tuple_ty(&mut self, node: &'cx ast::TupleTy<'cx>) -> Self::Result {
        self.emitter.print().p_l_bracket();
        self.emit_list(
            node.tys,
            |this, item| {
                this.visit_ty(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_bracket();
    }

    fn visit_lit_ty(&mut self, node: &'cx ast::LitTy) -> Self::Result {
        use ast::LitTyKind::*;
        match &node.kind {
            Void => self.emitter.print().p("void"),
            Null => self.emitter.print().p("null"),
            True => self.emitter.print().p("true"),
            False => self.emitter.print().p("false"),
            Undefined => self.emitter.print().p("undefined"),
            Num(num) => self.emitter.print().p(&num.to_string()),
            String(atom) => self.emit_string_literal(*atom),
            BigInt { neg: _, val: _ } => todo!(),
        }
    }

    fn visit_param_decl(&mut self, node: &'cx ast::ParamDecl<'cx>) -> Self::Result {
        self.visit_binding(node.name);
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        if let Some(ty) = node.ty {
            self.visit_ty(ty);
        } else {
            self.emitter.print().p("any");
        }
    }

    fn visit_method_signature(&mut self, node: &'cx ast::MethodSignature<'cx>) -> Self::Result {
        self.visit_prop_name(node.name);
        self.emitter.print().p_l_paren();
        self.emit_list(
            node.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();

        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.emit_ret_ty(node.ty);
        self.emitter.print().p_semi();
    }

    // TODO: merge this and `JsEmitter::visit_prop_name`
    fn visit_prop_name(&mut self, node: &'cx ast::PropName<'cx>) -> Self::Result {
        use ast::PropNameKind::*;
        match &node.kind {
            Ident(n) => self.visit_ident(n),
            StringLit { raw, .. } => self.visit_string_lit(*raw),
            BigIntLit { .. } => todo!(),
            NumLit(_n) => todo!(),
            Computed(n) => {
                self.visit_computed_prop_name(n);
            }
            PrivateIdent(_private_ident) => todo!(),
        }
    }

    fn visit_computed_prop_name(&mut self, node: &'cx ast::ComputedPropName<'cx>) -> Self::Result {
        self.emitter.print().p_l_bracket();
        self.visit_expr(node.expr);
        self.emitter.print().p_r_bracket();
    }

    fn visit_string_lit(&mut self, node: &'cx ast::StringLit) -> Self::Result {
        self.emit_string_literal(node.val);
    }

    fn visit_prop_signature(&mut self, node: &'cx ast::PropSignature<'cx>) -> Self::Result {
        self.visit_prop_name(node.name);
        if node.question.is_some() {
            self.emitter.print().p_question();
        }
        if let Some(ty) = node.ty {
            self.emitter.print().p_colon();
            self.emitter.print().p_whitespace();
            self.visit_ty(ty);
        }
        self.emitter.print().p_semi();
    }

    fn visit_union_ty(&mut self, n: &'cx ast::UnionTy<'cx>) -> Self::Result {
        self.emit_list(
            n.tys,
            |this, item| {
                this.visit_ty(item);
            },
            |this, _| {
                this.emitter.print().p_whitespace();
                this.emitter.print().p_pipe();
                this.emitter.print().p_whitespace();
            },
        );
    }

    fn visit_array_ty(&mut self, node: &'cx ast::ArrayTy<'cx>) -> Self::Result {
        self.visit_ty(node.ele);
        self.emitter.print().p_l_bracket();
        self.emitter.print().p_r_bracket();
    }

    fn visit_refer_ty(&mut self, n: &'cx ast::ReferTy<'cx>) -> Self::Result {
        self.visit_entity_name(n.name);
        self.emit_type_arguments(n.ty_args);
    }

    fn visit_entity_name(&mut self, node: &'cx bolt_ts_ast::EntityName<'cx>) -> Self::Result {
        use ast::EntityNameKind::*;
        match &node.kind {
            Ident(n) => self.visit_ident(n),
            Qualified(q) => {
                self.visit_entity_name(q.left);
                self.emitter.print().p_dot();
                self.visit_ident(q.right);
            }
        }
    }

    fn visit_ident(&mut self, node: &'cx ast::Ident) -> Self::Result {
        self.emitter.emit_atom(self.resolver.atoms(), node.name);
    }

    fn visit_fn_decl(&mut self, n: &'cx ast::FnDecl<'cx>) -> Self::Result {
        if n.modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::EXPORT))
        {
            self.emitter.print().p("export");
            self.emitter.print().p_whitespace();
        }
        if n.modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::DEFAULT))
        {
            self.emitter.print().p("default");
            self.emitter.print().p_whitespace();
        }
        self.emit_declare_if_needed();
        self.emitter.print().p("function");
        self.emitter.print().p_whitespace();
        if let Some(name) = n.name.map(|name| name.name) {
            self.emitter.emit_atom(self.resolver.atoms(), name);
        }
        self.emit_type_parameters(n.ty_params);
        self.emitter.print().p_l_paren();
        self.emit_list(
            n.params,
            |this, item| {
                this.visit_param_decl(item);
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        if let Some(ty) = n.ty {
            self.visit_ty(ty);
        } else {
            let ty = self.resolver.ensure_type_for_function_declaration(n);
            let ty_str = self.resolver.print_type(ty);
            self.emitter.print().p(&ty_str);
        }
        self.emitter.print().p_semi();
    }

    fn visit_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) -> Self::Result {
        self.emit_declare_if_needed();
        self.emitter.print().p("enum");
        self.emitter.print().p_whitespace();
        self.emitter
            .emit_atom(self.resolver.atoms(), node.name.name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_brace();
        self.emitter.increment_indent();
        self.emitter.print().p_newline();
        self.emit_list(
            node.members,
            |this, item| {
                this.visit_enum_member(item);
                this.emitter.print().p_whitespace();
                this.emitter.print().p_eq();
                this.emitter.print().p_whitespace();
                let enum_member_value = this.resolver.get_enum_member_value(item);
                match enum_member_value {
                    bolt_ts_checker::check::EnumMemberValue::Number(num) => {
                        this.emitter.print().p(&num.to_string());
                    }
                    bolt_ts_checker::check::EnumMemberValue::Str(str) => {
                        this.emitter.emit_atom(this.resolver.atoms(), str);
                    }
                    bolt_ts_checker::check::EnumMemberValue::Err => {
                        this.emitter.print().p("/* computed value */");
                    }
                };
            },
            |this, _| {
                this.emitter.print().p_comma();
                this.emitter.print().p_newline();
            },
        );
        self.emitter.decrement_indent();
        self.emitter.print().p_newline();
        self.emitter.print().p_r_brace();
    }

    fn visit_var_decl(&mut self, node: &'cx ast::VarDecl<'cx>) -> Self::Result {
        let ast::BindingKind::Ident(name) = node.name.kind else {
            todo!()
        };
        self.emit_declare_if_needed();
        let node_flags = self.resolver.node_flags(node.id);
        if node_flags.contains(ast::NodeFlags::CONST) {
            self.emitter.print().p("const");
        } else if node_flags.contains(ast::NodeFlags::LET) {
            self.emitter.print().p("let");
        } else {
            self.emitter.print().p("var");
        }
        self.emitter.print().p_whitespace();
        self.emitter.emit_atom(self.resolver.atoms(), name.name);
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        if let Some(ty) = node.ty {
            self.visit_ty(ty);
        } else {
            let ty = self.resolver.ensure_type_for_variable_declaration(node);
            let ty_str = self.resolver.print_type(ty);
            self.emitter.print().p(&ty_str);
        }
        self.emitter.print().p_semi();
        self.emitter.print().p_newline();
    }

    fn visit_typeof_ty(&mut self, node: &'cx ast::TypeofTy<'cx>) -> Self::Result {
        self.emitter.print().p("typeof");
        self.emitter.print().p_whitespace();
        self.visit_entity_name(node.name);
        self.emit_type_arguments(node.ty_args);
    }

    fn visit_export_assign(&mut self, node: &'cx ast::ExportAssign<'cx>) -> Self::Result {
        if node.is_export_equals {
            self.emitter.print().p("export =");
        } else {
            self.emitter.print().p("export default");
        }
        self.emitter.print().p_whitespace();
        self.visit_expr(node.expr);
        self.emitter.print().p_semi();
    }

    fn visit_assign_expr(&mut self, _: &'cx ast::AssignExpr<'cx>) -> Self::Result {}

    fn visit_mapped_ty(&mut self, node: &'cx ast::MappedTy<'cx>) -> Self::Result {
        self.emitter.print().p_l_brace();
        self.emitter.print().p_newline();
        self.emitter.print().p_l_bracket();
        self.visit_ident(node.ty_param.name);
        if let Some(constraint) = node.ty_param.constraint {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("in");
            self.emitter.print().p_whitespace();
            self.visit_ty(constraint);
        }
        if let Some(name_ty) = node.name_ty {
            self.visit_ty(name_ty);
        }
        self.emitter.print().p_r_bracket();
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        if let Some(ty) = node.ty {
            self.visit_ty(ty);
        }
        self.emitter.print().p_newline();
        self.emitter.print().p_r_brace();
    }

    fn visit_indexed_access_ty(&mut self, node: &'cx ast::IndexedAccessTy<'cx>) -> Self::Result {
        self.visit_ty(node.ty);
        self.emitter.print().p_l_bracket();
        self.visit_ty(node.index_ty);
        self.emitter.print().p_r_bracket();
    }

    fn visit_ty_op_ty(&mut self, node: &'cx ast::TypeOp<'cx>) -> Self::Result {
        match node.op {
            ast::TyOpKind::Keyof => self.emitter.print().p("keyof"),
            ast::TyOpKind::Readonly => self.emitter.print().p("readonly"),
            ast::TyOpKind::Unique => self.emitter.print().p("unique"),
        }
        self.emitter.print().p_whitespace();
        self.visit_ty(node.ty);
    }

    fn visit_paren_ty(&mut self, node: &'cx ast::ParenTy<'cx>) -> Self::Result {
        self.emitter.print().p_l_paren();
        self.visit_ty(node.ty);
        self.emitter.print().p_r_paren();
    }

    fn visit_intersection_ty(&mut self, node: &'cx ast::IntersectionTy<'cx>) -> Self::Result {
        self.emit_list(
            node.tys,
            |this, item| {
                this.visit_ty(item);
            },
            |this, _| {
                this.emitter.print().p_whitespace();
                this.emitter.print().p_ampersand();
                this.emitter.print().p_whitespace();
            },
        );
    }

    fn visit_import_equals_decl(
        &mut self,
        node: &'cx bolt_ts_ast::ImportEqualsDecl<'cx>,
    ) -> Self::Result {
        if let Some(_) = node.export_modifier {
            self.emitter.print().p("export");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("import");
        self.emitter.print().p_whitespace();
        self.visit_ident(node.name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_eq();
        self.emitter.print().p_whitespace();
        match node.module_reference {
            ast::ModuleReferenceKind::EntityName(n) => {
                self.visit_entity_name(n);
            }
            ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                self.visit_string_lit(n.module_spec());
            }
        }
        self.emitter.print().p_semi();
    }
}
