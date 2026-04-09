use bolt_ts_ast::{self as ast};
use bolt_ts_ast_visitor::Visitor;
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
    let resolver = EmitResolver::new(checker);
    let mut emitter = DeclarationEmitter { emitter, resolver };
    let root = emitter.resolver.program(module_id);
    emitter.visit_program(root);
    emitter.emitter.print().take_content()
}

struct DeclarationEmitter<'cx, 'a> {
    emitter: Emitter,
    resolver: EmitResolver<'cx, 'a>,
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
            Some(ty) => self.visit_ty(ty),
            None => self.emitter.print().p("any"),
        }
    }
}

impl<'cx, 'a> bolt_ts_ast_visitor::Visitor<'cx> for DeclarationEmitter<'cx, 'a> {
    fn visit_program(&mut self, node: &'cx bolt_ts_ast::Program<'cx>) {
        for stmt in node.stmts() {
            self.visit_stmt(stmt);
            self.emitter.print().p_newline();
        }
    }

    fn visit_ty_param(&mut self, node: &'cx bolt_ts_ast::TyParam<'cx>) {
        self.visit_ident(node.name);
        if let Some(constraint) = node.constraint {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("extends");
            self.emitter.print().p_whitespace();
            self.visit_ty(constraint);
        }
    }

    fn visit_object_lit_ty(&mut self, node: &'cx bolt_ts_ast::ObjectLitTy<'cx>) {
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

    fn visit_class_decl(&mut self, node: &'cx bolt_ts_ast::ClassDecl<'cx>) {
        self.emitter.print().p("declare");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("class");
        self.emitter.print().p_whitespace();
        if let Some(name) = node.name.map(|name| name.name) {
            self.emitter.emit_atom(self.resolver.atoms(), name);
        }
        if let Some(type_params) = node.ty_params {
            if !type_params.is_empty() {
                self.emitter.print().p_less();
                for type_param in type_params {
                    self.visit_ty_param(type_param);
                }
                self.emitter.print().p_great();
            }
        }
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_brace();
        if !node.elems.list.is_empty() {
            self.emitter.increment_indent();
            self.emitter.print().p_newline();
            self.emit_list(
                node.elems.list,
                |this, elem| {
                    use ast::ClassElemKind::*;
                    match elem.kind {
                        Ctor(_n) => todo!(),
                        Prop(_n) => todo!(),
                        Method(_n) => todo!(),
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

    fn visit_setter_decl(&mut self, node: &'cx bolt_ts_ast::SetterDecl<'cx>) {
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

    fn visit_index_sig_decl(&mut self, node: &'cx ast::IndexSigDecl<'cx>) {
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

    fn visit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) {
        self.emitter.print().p("interface");
        self.emitter.print().p_whitespace();
        self.emitter
            .emit_atom(self.resolver.atoms(), node.name.name);
        self.emitter.print().p_whitespace();
        if let Some(extends) = node.extends {
            self.emitter.print().p("extends");
            self.emitter.print().p_whitespace();
            for item in extends.list {
                self.visit_refer_ty(item);
            }
        }
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

    fn visit_fn_ty(&mut self, node: &'cx bolt_ts_ast::FnTy<'cx>) {
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

    fn visit_type_alias_decl(&mut self, node: &'cx bolt_ts_ast::TypeAliasDecl<'cx>) {
        self.emitter.print().p("type");
        self.emitter.print().p_whitespace();
        self.emitter
            .emit_atom(self.resolver.atoms(), node.name.name);
        if let Some(type_params) = node.ty_params
            && !type_params.is_empty()
        {
            self.emitter.print().p_less();
            for type_param in type_params {
                self.visit_ident(type_param.name);
            }
            self.emitter.print().p_great();
        }
        self.emitter.print().p_whitespace();
        self.emitter.print().p_eq();
        self.emitter.print().p_whitespace();
        self.visit_ty(node.ty);
        self.emitter.print().p_semi();
    }

    fn visit_lit_ty(&mut self, node: &'cx bolt_ts_ast::LitTy) {
        use bolt_ts_ast::LitTyKind::*;
        match &node.kind {
            Void => self.emitter.print().p("void"),
            Null => self.emitter.print().p("null"),
            True => self.emitter.print().p("true"),
            False => self.emitter.print().p("false"),
            Undefined => self.emitter.print().p("undefined"),
            Num(num) => self.emitter.print().p(&num.to_string()),
            String(atom) => self.emitter.emit_atom(self.resolver.atoms(), *atom),
            BigInt { neg: _, val: _ } => todo!(),
        }
    }

    fn visit_param_decl(&mut self, node: &'cx bolt_ts_ast::ParamDecl<'cx>) {
        self.visit_binding(node.name);
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        if let Some(ty) = node.ty {
            self.visit_ty(ty);
        } else {
            self.emitter.print().p("any");
        }
    }

    fn visit_method_signature(&mut self, node: &'cx bolt_ts_ast::MethodSignature<'cx>) {
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
    fn visit_prop_name(&mut self, node: &'cx bolt_ts_ast::PropName<'cx>) {
        use bolt_ts_ast::PropNameKind::*;
        match &node.kind {
            Ident(n) => self.visit_ident(n),
            StringLit { raw, .. } => self.visit_string_lit(*raw),
            BigIntLit { .. } => todo!(),
            NumLit(_n) => todo!(),
            Computed(_n) => {
                todo!()
            }
            PrivateIdent(_private_ident) => todo!(),
        }
    }

    fn visit_string_lit(&mut self, node: &'cx bolt_ts_ast::StringLit) {
        self.emitter.print().p_double_quote();
        self.emitter.emit_atom(self.resolver.atoms(), node.val);
        self.emitter.print().p_double_quote();
    }

    fn visit_prop_signature(&mut self, node: &'cx ast::PropSignature<'cx>) {
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

    fn visit_union_ty(&mut self, n: &'cx bolt_ts_ast::UnionTy<'cx>) {
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

    fn visit_array_ty(&mut self, node: &'cx bolt_ts_ast::ArrayTy<'cx>) {
        self.visit_ty(node.ele);
        self.emitter.print().p_l_bracket();
        self.emitter.print().p_r_bracket();
    }

    fn visit_refer_ty(&mut self, n: &'cx ast::ReferTy<'cx>) {
        self.visit_entity_name(n.name);
        if let Some(ty_args) = n.ty_args
            && !ty_args.list.is_empty()
        {
            self.emitter.print().p_less();
            for ty in ty_args.list {
                self.visit_ty(ty);
            }
            self.emitter.print().p_great();
        }
    }

    fn visit_ident(&mut self, node: &'cx ast::Ident) {
        self.emitter.emit_atom(self.resolver.atoms(), node.name);
    }

    fn visit_fn_decl(&mut self, node: &'cx bolt_ts_ast::FnDecl<'cx>) {
        if node
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::EXPORT))
        {
            self.emitter.print().p("export");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("declare");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("function");
        self.emitter.print().p_whitespace();
        if let Some(name) = node.name.map(|name| name.name) {
            self.emitter.emit_atom(self.resolver.atoms(), name);
        }
        if let Some(type_params) = node.ty_params
            && !type_params.is_empty()
        {
            self.emitter.print().p_less();
            for type_param in type_params {
                self.visit_ident(type_param.name);
            }
            self.emitter.print().p_great();
        }
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
            let ty = self.resolver.ensure_type_for_function_declaration(node);
            let ty_str = self.resolver.print_type(ty);
            self.emitter.print().p(&ty_str);
        }
        self.emitter.print().p_semi();
    }

    fn visit_enum_decl(&mut self, node: &'cx bolt_ts_ast::EnumDecl<'cx>) {
        self.emitter.print().p("declare");
        self.emitter.print().p_whitespace();
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

    fn visit_var_decl(&mut self, node: &'cx bolt_ts_ast::VarDecl<'cx>) {
        let ast::BindingKind::Ident(name) = node.name.kind else {
            todo!()
        };
        self.emitter.print().p("declare");
        self.emitter.print().p_whitespace();
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
    }
}
