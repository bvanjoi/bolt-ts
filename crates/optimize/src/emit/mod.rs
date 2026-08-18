mod print;

use std::borrow::Cow;

use bolt_ts_ast as ast;
use bolt_ts_ast_visitor::{Visitor, noop_visit_type_node};
use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_checker::emit_resolver::EmitResolver;
use bolt_ts_span::ModuleID;
use rustc_hash::FxHashSet;

use super::emit::print::PPrint;

#[derive(Clone, Copy)]
pub struct EmitterOptions {
    indent: u32,
}

bolt_ts_utils::index! {
    ScopeID
}

pub fn emit_js<'cx, 'a>(
    resolver: EmitResolver<'cx, 'a>,
    module: ModuleID,
    origin: String,
) -> String {
    let emitter = Emitter::new();
    let scope = ScopeID::root();
    let max_scope = ScopeID::root().next();
    let mut js_emitter = JSEmitter {
        emitter,
        resolver,
        ns_names: FxHashSet::default(),
        scope,
        max_scope,
        origin,
    };
    let root = js_emitter.resolver.program(module);
    js_emitter.visit_program(root);
    js_emitter.emitter.print().take_content()
}

pub struct Emitter {
    options: EmitterOptions,
    content: PPrint,
}

impl Default for Emitter {
    fn default() -> Self {
        Self::new()
    }
}

impl Emitter {
    pub fn new() -> Self {
        Self {
            options: EmitterOptions { indent: 2 },
            content: PPrint::new(1024),
        }
    }

    #[inline]
    pub fn print(&mut self) -> &mut PPrint {
        &mut self.content
    }

    #[inline]
    pub fn emit_atom(&mut self, atoms: &AtomIntern, atom: Atom) {
        let name = atoms.get(atom);
        self.print().p(name);
    }

    #[inline(always)]
    pub fn increment_indent(&mut self) {
        self.print().indent += self.options.indent;
    }

    #[inline(always)]
    pub fn decrement_indent(&mut self) {
        self.print().indent -= self.options.indent;
    }
}

struct JSEmitter<'cx, 'a> {
    emitter: Emitter,
    resolver: EmitResolver<'cx, 'a>,
    ns_names: FxHashSet<(ScopeID, bolt_ts_atom::Atom)>,
    scope: ScopeID,
    max_scope: ScopeID,
    origin: String,
}

impl<'cx, 'a> JSEmitter<'cx, 'a> {
    fn next_scope(&mut self) -> ScopeID {
        let scope = self.max_scope;
        self.max_scope = self.max_scope.next();
        scope
    }

    fn atoms(&self) -> &AtomIntern {
        self.resolver.atoms()
    }

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

    fn emit_var_decls(&mut self, decls: ast::VarDecls<'cx>) {
        self.emit_list(
            decls,
            |this, decl| this.visit_var_decl(decl),
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_whitespace();
            },
        );
    }

    fn emit_as_string(&mut self, val: Atom) {
        let s = self.atoms().get(val);
        self.emitter.print().p("'");
        for c in s.chars() {
            match c {
                '\'' => self.emitter.print().p("\\'"),
                _ => self.emitter.print().content.push(c),
            }
        }
        self.emitter.print().p("'");
    }

    fn emit_params(&mut self, params: ast::ParamsDecl<'cx>) {
        self.emitter.print().p_l_paren();
        self.emit_list(
            params,
            |this, item| this.visit_param_decl(item),
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
    }

    fn emit_args(&mut self, args: ast::Exprs<'cx>) {
        self.emitter.print().p_l_paren();
        self.emit_list(
            args,
            |this, arg| this.visit_expr(arg),
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
    }

    fn emit_export_modifier_if_root(&mut self, modifiers: Option<&'cx ast::Modifiers<'cx>>) {
        if let Some(ms) = modifiers {
            if self.scope == ScopeID::root() && ms.flags.contains(ast::ModifierFlags::EXPORT) {
                self.emitter.print().p("export");
                self.emitter.print().p_whitespace();
            }
            if self.scope == ScopeID::root() && ms.flags.contains(ast::ModifierFlags::DEFAULT) {
                self.emitter.print().p("default");
                self.emitter.print().p_whitespace();
            }
        }
    }

    fn emit_static_modifier(&mut self, modifiers: Option<&'cx ast::Modifiers<'cx>>) {
        if let Some(ms) = modifiers
            && ms.flags.contains(ast::ModifierFlags::STATIC)
        {
            self.emitter.print().p("static");
            self.emitter.print().p_whitespace();
        }
    }

    fn emit_class_extends_clause(&mut self, extends: &'cx ast::ClassExtendsClause<'cx>) {
        self.emitter.print().p("extends");
        self.emitter.print().p_whitespace();
        self.visit_expr_with_ty_args(extends.expr_with_ty_args);
        self.emitter.print().p_whitespace();
    }

    fn emit_with_var_fn_wrapper(
        &mut self,
        decl_name: &'cx ast::Ident,
        param_name: &str,
        f: impl FnOnce(&mut Self),
    ) {
        let name = decl_name.name;
        if self.ns_names.insert((self.scope, name)) {
            self.emitter.print().p("var");
            self.emitter.print().p_whitespace();
            self.visit_ident(decl_name);
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.emitter.print().p("{}");
            self.emitter.print().p_semi();
        }

        self.emitter.print().p_newline();

        self.emitter.print().p_l_paren();
        self.emitter.print().p("function");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_paren();

        self.emitter.print().p(param_name);
        self.emitter.print().p_r_paren();
        self.emitter.print().p_whitespace();

        // emit block
        self.emitter.print().p_l_brace();
        self.emitter.print().p_newline();
        self.emitter.print().indent += self.emitter.options.indent;
        let old = self.scope;
        self.scope = self.next_scope();

        f(self);

        self.scope = old;
        self.emitter.print().indent -= self.emitter.options.indent;
        self.emitter.print().p_newline();
        self.emitter.print().p_r_brace();

        self.emitter.print().p_r_paren();
        self.emitter.print().p_l_paren();
        self.visit_ident(decl_name);
        self.emitter.print().p_r_paren();
        self.emitter.print().p_semi();
    }

    fn sub_names_of_binding(&self, binding: &'cx ast::Binding<'cx>) -> Vec<Atom> {
        match binding.kind {
            ast::BindingKind::Ident(n) => vec![n.name],
            ast::BindingKind::ObjectPat(n) => n
                .elems
                .iter()
                .flat_map(|elem| match elem.name {
                    ast::ObjectBindingName::Shorthand(ident) => vec![ident.name],
                    ast::ObjectBindingName::Prop { name, .. } => self.sub_names_of_binding(name),
                })
                .collect(),
            ast::BindingKind::ArrayPat(n) => n
                .elems
                .iter()
                .flat_map(|elem| match elem.kind {
                    ast::ArrayBindingElemKind::Omit(_) => vec![],
                    ast::ArrayBindingElemKind::Binding(n) => self.sub_names_of_binding(n.name),
                })
                .collect(),
        }
    }

    fn is_param_property(&self, param: &'cx ast::ParamDecl<'cx>) -> bool {
        param.dotdotdot.is_none()
            && param
                .modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::PUBLIC))
    }

    fn emit_module_block_contents(&mut self, block: &'cx ast::ModuleBlock<'cx>, param_name: &str) {
        for stmt in block.stmts {
            if self.stmt_is_omitted(stmt) {
                continue;
            }
            self.emitter.content.p_newline();
            self.visit_stmt(stmt);
            self.emitter.content.p_newline();
            let t = match stmt.kind {
                ast::StmtKind::Var(v) => {
                    if let Some(ms) = v.modifiers
                        && ms.flags.contains(ast::ModifierFlags::EXPORT)
                        && !ms.flags.contains(ast::ModifierFlags::AMBIENT)
                    {
                        for item in v.list {
                            self.emitter.content.p(param_name);
                            self.emitter.content.p_dot();
                            self.visit_binding(item.name);
                            self.emitter.content.p_whitespace();
                            self.emitter.content.p_eq();
                            self.emitter.content.p_whitespace();
                            self.visit_binding(item.name);
                            self.emitter.content.p_newline();
                        }
                    }
                    continue;
                }
                ast::StmtKind::Fn(f) => {
                    let Some(name) = f.name else {
                        continue;
                    };
                    f.modifiers.map(|ms| (ms, name))
                }
                ast::StmtKind::Class(c) => {
                    if let Some(name) = c.name {
                        c.modifiers.map(|ms| (ms, name))
                    } else {
                        return;
                    }
                }
                ast::StmtKind::NestedModule(n) => n.modifiers.map(|ms| {
                    let ident = n.name;
                    (ms, ident)
                }),
                ast::StmtKind::BlockModule(n) => {
                    let ident = match n.name {
                        ast::ModuleName::Ident(ident) => ident,
                        ast::ModuleName::StringLit(_) => unreachable!(),
                    };
                    n.modifiers.map(|ms| (ms, ident))
                }
                ast::StmtKind::Enum(n) => n.modifiers.map(|ms| (ms, n.name)),
                _ => None,
            };
            let Some((ms, name)) = t else {
                continue;
            };
            if ms.flags.contains(ast::ModifierFlags::EXPORT)
                && !ms.flags.contains(ast::ModifierFlags::AMBIENT)
            {
                self.emitter.content.p(param_name);
                self.emitter.content.p_dot();
                self.visit_ident(name);
                self.emitter.content.p_whitespace();
                self.emitter.content.p_eq();
                self.emitter.content.p_whitespace();
                self.visit_ident(name);
                self.emitter.content.p_semi();
                self.emitter.content.p_newline();
            }
        }
    }

    fn stmt_is_omitted(&self, stmt: &'cx ast::Stmt<'cx>) -> bool {
        use ast::StmtKind::*;
        match stmt.kind {
            Interface(_) | TypeAlias(_) | Debugger(_) => true,
            Class(n) => n
                .modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT)),
            Fn(n) => n.body.is_none(),
            _ => false,
        }
    }

    fn emit_stmts_skip_omitted(&mut self, stmts: &'cx [&'cx ast::Stmt<'cx>]) {
        let mut first = true;
        for stmt in stmts {
            if self.stmt_is_omitted(stmt) {
                continue;
            }
            if !first {
                self.emitter.content.p_newline();
            }
            self.visit_stmt(stmt);
            first = false;
        }
    }

    fn class_elem_is_empty(&self, elem: &'cx ast::ClassElem<'cx>) -> bool {
        use ast::ClassElemKind::*;
        match elem.kind {
            IndexSig(_) | Semi(_) => true,
            Ctor(n) => n.body.is_none(),
            Method(n) => n.body.is_none(),
            Getter(n) => n.body.is_none(),
            Setter(n) => n.body.is_none(),
            _ => false,
        }
    }

    fn nested_module_instantiated(&self, node: &'cx ast::NestedModuleDecl<'cx>) -> bool {
        match node.block {
            ast::NestedModuleBlock::Nested(inner) => self.nested_module_instantiated(inner),
            ast::NestedModuleBlock::Block(block) => {
                self.resolver
                    .is_module_instantiated(node.id.module(), Some(block), node.id)
            }
        }
    }

    fn is_this_param(&self, param: &'cx ast::ParamDecl<'cx>) -> bool {
        matches!(
            param.name.kind,
            ast::BindingKind::Ident(ident) if ident.name == ast::keyword::KW_THIS
        )
    }

    fn emit_params_without_this(&mut self, params: ast::ParamsDecl<'cx>) {
        self.emitter.print().p_l_paren();
        let filtered: Vec<_> = params
            .iter()
            .enumerate()
            .filter_map(|(idx, param)| {
                if idx == 0 && self.is_this_param(param) {
                    None
                } else {
                    Some(*param)
                }
            })
            .collect();
        self.emit_list(
            &filtered,
            |this, item| this.visit_param_decl(item),
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
    }

    fn emit_class_body(&mut self, elems: &'cx ast::ClassElems<'cx>) {
        let items: Vec<_> = elems
            .list
            .iter()
            .filter(|e| !self.class_elem_is_empty(e))
            .copied()
            .collect();
        self.emitter.print().p_l_brace();
        if !items.is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
            self.emit_list(
                &items,
                |this, elem| {
                    this.visit_class_elem(elem);
                },
                |this, _| {
                    this.emitter.content.p_newline();
                },
            );
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn emit_enum_member_name(&mut self, name: &'cx ast::EnumMemberNameKind<'cx>) {
        use ast::EnumMemberNameKind::*;
        match name {
            Ident(ident) => self.emit_as_string(ident.name),
            StringLit { raw, .. } => self.visit_string_lit(raw),
        }
    }

    fn visit_import_spec(&mut self, node: &'cx ast::ImportSpec<'cx>) {
        match node.kind {
            ast::ImportSpecKind::Shorthand(n) => self.visit_import_shorthand_spec(n),
            ast::ImportSpecKind::Named(n) => self.visit_import_named_spec(n),
        }
    }

    fn emit_for_init(&mut self, init: ast::ForInitKind<'cx>) {
        match init {
            ast::ForInitKind::Var(decls) => {
                self.emitter.print().p("var");
                self.emitter.print().p_whitespace();
                self.emit_var_decls(decls);
            }
            ast::ForInitKind::Expr(expr) => self.visit_expr(expr),
        }
    }

    fn visit_export_spec(&mut self, node: &'cx ast::ExportSpec<'cx>) {
        match node.kind {
            ast::ExportSpecKind::Shorthand(n) => self.visit_export_shorthand_spec(n),
            ast::ExportSpecKind::Named(n) => self.visit_export_named_spec(n),
        }
    }

    fn visit_object_member(&mut self, node: &'cx ast::ObjectMember<'cx>) {
        use ast::ObjectMemberKind::*;
        match node.kind {
            PropAssignment(n) => self.visit_object_prop_assignment(n),
            Shorthand(n) => self.visit_object_shorthand_member(n),
            Method(n) => self.visit_object_method_member(n),
            SpreadAssignment(n) => {
                self.emitter.print().p("...");
                self.visit_expr(n.expr);
            }
            Getter(n) => self.visit_getter_decl(n),
            Setter(n) => self.visit_setter_decl(n),
        }
    }

    fn visit_jsx_tag_name(&mut self, node: ast::JsxTagName<'cx>) {
        use ast::JsxTagName::*;
        match node {
            Ident(n) => self.visit_ident(n),
            Ns(n) => self.visit_jsx_ns_name(n),
            PropAccess(n) => self.visit_prop_access_expr(n),
            This(_) => self.emitter.print().p("this"),
        }
    }

    fn visit_jsx_attr(&mut self, node: &'cx ast::JsxAttr<'cx>) {
        match node {
            ast::JsxAttr::Spread(n) => {
                self.emitter.print().p_l_brace();
                self.emitter.print().p_dot_dot_dot();
                self.visit_expr(n.expr);
                self.emitter.print().p_r_brace();
            }
            ast::JsxAttr::Named(n) => {
                self.visit_jsx_attr_name(n.name.clone());
                if let Some(v) = n.init {
                    self.emitter.print().p_eq();
                    self.visit_jsx_attr_value(v);
                }
            }
        }
    }

    fn visit_jsx_attr_name(&mut self, node: ast::JsxAttrName<'cx>) {
        use ast::JsxAttrName::*;
        match node {
            Ident(n) => self.visit_ident(n),
            Ns(n) => self.visit_jsx_ns_name(n),
        }
    }

    fn visit_jsx_attr_value(&mut self, node: ast::JsxAttrValue<'cx>) {
        use ast::JsxAttrValue::*;
        match node {
            StringLit(n) => self.visit_string_lit(n),
            Expr(n) => self.visit_jsx_expr(n),
            Ele(n) => self.visit_jsx_elem(n),
            SelfClosingEle(n) => self.visit_jsx_self_closing_elem(n),
            Frag(n) => self.visit_jsx_frag(n),
        }
    }

    fn visit_jsx_child(&mut self, child: ast::JsxChild<'cx>) {
        use ast::JsxChild::*;
        match child {
            Text(n) => {
                let content = self.atoms().get(n.text);
                self.emitter.print().p(content);
            }
            Expr(n) => self.visit_jsx_expr(n),
            Elem(n) => self.visit_jsx_elem(n),
            SelfClosingEle(n) => self.visit_jsx_self_closing_elem(n),
            Frag(n) => self.visit_jsx_frag(n),
        }
    }

    fn emit_block_module_decl(&mut self, node: &ast::BlockModuleDecl<'cx>) {
        if node
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
        {
            return;
        }
        let Some(block) = node.block else {
            return;
        };
        let module = node.id.module();
        if !self
            .resolver
            .is_module_instantiated(module, Some(block), node.id)
        {
            return;
        }

        let ident = match node.name {
            ast::ModuleName::Ident(ident) => ident,
            ast::ModuleName::StringLit(_) => return,
        };

        let mut sub_names: Vec<_> = block
            .stmts
            .iter()
            .filter_map(|stmt| match stmt.kind {
                ast::StmtKind::Var(v) => Some(
                    v.list
                        .iter()
                        .flat_map(|item| self.sub_names_of_binding(item.name))
                        .collect::<Vec<_>>(),
                ),
                ast::StmtKind::Class(c) => c.name.map(|name| vec![name.name]),
                ast::StmtKind::Fn(f) => f.name.map(|name| vec![name.name]),
                _ => None,
            })
            .flatten()
            .map(|name| self.atoms().get(name))
            .collect();
        sub_names.sort();

        let mut param_name = Cow::Borrowed(self.atoms().get(ident.name));
        if let Some(i) = sub_names.iter().position(|sub| *sub == param_name) {
            let mut offset = 1;
            let mut n = format!("{param_name}_{offset}");
            for sub in &sub_names[i + 1..] {
                if n == *sub {
                    offset += 1;
                    n = format!("{n}_{offset}");
                } else {
                    break;
                }
            }
            param_name = Cow::Owned(n);
        }

        self.emit_with_var_fn_wrapper(ident, &param_name, |this| {
            this.emit_module_block_contents(block, &param_name);
        });
    }

    fn emit_leading_comments(&mut self, pos: u32) {
        if self.resolver.config().compiler_options().remove_comments() {
            return;
        }
        bolt_ts_scanner::iterate_comment_ranges::<false, false>(
            &self.origin,
            pos as usize,
            |kind, start, end, has_trailing_newline| {
                let comment = &self.origin[start..end];
                self.emitter.print().p(comment);
                if has_trailing_newline {
                    self.emitter.print().p_newline();
                } else if matches!(kind, bolt_ts_scanner::CommentKind::MultiLine) {
                    self.emitter.print().p_whitespace();
                }
                false
            },
        );
    }
}

impl<'cx, 'a> Visitor<'cx> for JSEmitter<'cx, 'a> {
    type Result = ();

    fn visit_program(&mut self, node: &'cx ast::Program<'cx>) -> Self::Result {
        let span = node.span();
        self.emit_leading_comments(span.lo());
        let mut first = true;
        for stmt in node.stmts() {
            if self.stmt_is_omitted(stmt) {
                continue;
            }
            if !first {
                self.emitter.content.p_newline();
            }
            self.visit_stmt(stmt);
            first = false;
        }
    }

    fn visit_var_stmt(&mut self, node: &'cx ast::VarStmt<'cx>) -> Self::Result {
        if node
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
        {
            return;
        }
        self.emitter.print().p("var");
        self.emitter.print().p_whitespace();
        self.emit_var_decls(node.list);
        self.emitter.print().p_semi();
    }

    fn visit_var_decl(&mut self, node: &'cx ast::VarDecl<'cx>) -> Self::Result {
        self.visit_binding(node.name);
        if let Some(init) = node.init {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.visit_expr(init);
        }
    }

    fn visit_ident(&mut self, node: &'cx ast::Ident) -> Self::Result {
        let content = self.atoms().get(node.name);
        self.emitter.print().p(content);
    }

    fn visit_private_ident(&mut self, node: &'cx ast::PrivateIdent) -> Self::Result {
        let content = self.atoms().get(node.name);
        self.emitter.print().p(content);
    }

    fn visit_num_lit(&mut self, node: &'cx ast::NumLit) -> Self::Result {
        self.emitter.print().p(&node.val.to_string())
    }

    fn visit_string_lit(&mut self, node: &'cx ast::StringLit) -> Self::Result {
        self.emit_as_string(node.val);
    }

    fn visit_prop_name(&mut self, node: &'cx ast::PropName<'cx>) -> Self::Result {
        use ast::PropNameKind::*;
        match &node.kind {
            Ident(n) => self.visit_ident(n),
            NumLit(n) => self.visit_num_lit(n),
            StringLit { raw, .. } => self.visit_string_lit(raw),
            Computed(n) => {
                self.emitter.print().p_l_bracket();
                self.visit_expr(n.expr);
                self.emitter.print().p_r_bracket();
            }
            PrivateIdent(n) => self.visit_private_ident(n),
            BigIntLit(n) => {
                let content = self.atoms().get(n.val.1);
                self.emitter.print().p(content);
            }
        }
    }

    fn visit_object_binding_elem(
        &mut self,
        node: &'cx ast::ObjectBindingElem<'cx>,
    ) -> Self::Result {
        if node.dotdotdot.is_some() {
            self.emitter.print().p_dot_dot_dot();
        }
        match node.name {
            ast::ObjectBindingName::Shorthand(ident) => {
                self.visit_ident(ident);
            }
            ast::ObjectBindingName::Prop { prop_name, name } => {
                self.visit_prop_name(prop_name);
                self.emitter.print().p(":");
                self.emitter.print().p_whitespace();
                self.visit_binding(name);
            }
        }
        if let Some(init) = node.init {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.visit_expr(init);
        }
    }

    fn visit_array_binding(&mut self, node: &'cx ast::ArrayBinding<'cx>) -> Self::Result {
        if node.dotdotdot.is_some() {
            self.emitter.print().p_dot_dot_dot();
        }
        self.visit_binding(node.name);
        if let Some(init) = node.init {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.visit_expr(init);
        }
    }

    fn visit_binding(&mut self, node: &'cx ast::Binding<'cx>) -> Self::Result {
        use ast::BindingKind::*;
        match node.kind {
            Ident(n) => self.visit_ident(n),
            ObjectPat(n) => {
                self.emitter.print().p_l_brace();
                self.emit_list(
                    n.elems,
                    |this, item| this.visit_object_binding_elem(item),
                    |this, _| {
                        this.emitter.content.p_comma();
                        this.emitter.content.p_whitespace();
                    },
                );
                self.emitter.print().p_r_brace();
            }
            ArrayPat(n) => {
                self.emitter.print().p_l_bracket();
                self.emit_list(
                    n.elems,
                    |this, item| match item.kind {
                        ast::ArrayBindingElemKind::Omit(_) => {}
                        ast::ArrayBindingElemKind::Binding(n) => {
                            this.visit_array_binding(n);
                        }
                    },
                    |this, _| {
                        this.emitter.content.p_comma();
                        this.emitter.content.p_whitespace();
                    },
                );
                self.emitter.print().p_r_bracket();
            }
        }
    }

    fn visit_param_decl(&mut self, node: &'cx ast::ParamDecl<'cx>) -> Self::Result {
        if node.dotdotdot.is_some() {
            self.emitter.print().p_dot_dot_dot();
        }
        self.visit_binding(node.name);
        if let Some(init) = node.init {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.visit_expr(init);
        }
    }

    fn visit_block_stmt(&mut self, node: &'cx ast::BlockStmt<'cx>) -> Self::Result {
        self.emitter.print().p_l_brace();
        let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
        if has_stmts {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_stmts_skip_omitted(node.stmts);
        if has_stmts {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn visit_if_stmt(&mut self, node: &'cx ast::IfStmt<'cx>) -> Self::Result {
        self.emitter.print().p("if");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_paren();
        self.visit_expr(node.expr);
        self.emitter.print().p_r_paren();
        self.emitter.print().p_whitespace();
        self.visit_stmt(node.then);
        if let Some(else_then) = node.else_then {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("else");
            self.emitter.print().p_whitespace();
            self.visit_stmt(else_then);
        }
        self.emitter.print().p_newline();
    }

    fn visit_ret_stmt(&mut self, node: &'cx ast::RetStmt<'cx>) -> Self::Result {
        self.emitter.print().p("return");
        self.emitter.print().p_whitespace();
        if let Some(expr) = node.expr {
            self.visit_expr(expr);
        }
    }

    fn visit_getter_decl(&mut self, node: &'cx ast::GetterDecl<'cx>) -> Self::Result {
        let Some(body) = node.body else {
            return;
        };
        self.emit_static_modifier(node.modifiers);
        self.emitter.print().p("get");
        self.emitter.print().p_whitespace();
        self.visit_prop_name(node.name);
        self.emit_params(&[]);
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(body);
    }

    fn visit_setter_decl(&mut self, node: &'cx ast::SetterDecl<'cx>) -> Self::Result {
        let Some(body) = node.body else {
            return;
        };
        self.emit_static_modifier(node.modifiers);
        self.emitter.print().p("set");
        self.emitter.print().p_whitespace();
        self.visit_prop_name(node.name);
        self.emit_params_without_this(node.params);
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(body);
    }

    fn visit_class_ctor(&mut self, node: &'cx ast::ClassCtor<'cx>) -> Self::Result {
        let Some(body) = node.body else {
            return;
        };
        self.emitter.print().p("constructor");
        self.emit_params_without_this(node.params);
        self.emitter.print().p_whitespace();

        self.emitter.print().p_l_brace();
        self.emitter.print().indent += self.emitter.options.indent;

        let has_block_stmt = body.stmts.iter().any(|s| !self.stmt_is_omitted(s))
            && node
                .params
                .iter()
                .any(|param| self.is_param_property(param));

        if has_block_stmt {
            self.emitter.print().p_newline();
        }

        let last_super_call = body.stmts.iter().rev().position(|stmt| {
            if let ast::StmtKind::Expr(expr_stmt) = stmt.kind
                && let ast::ExprKind::Call(call) = expr_stmt.expr.kind
            {
                return matches!(call.expr.kind, ast::ExprKind::Super(_));
            }
            false
        });
        let last_super_call = last_super_call.map(|pos| body.stmts.len() - 1 - pos);

        let (prev_stmts, after_stmts) = if let Some(last_super_call) = last_super_call {
            body.stmts.split_at(last_super_call + 1)
        } else {
            let after_stmts: &[&ast::Stmt<'cx>] = &[];
            (body.stmts, after_stmts)
        };

        self.emit_stmts_skip_omitted(prev_stmts);

        self.emit_list(
            node.params,
            |this, param| {
                if this.is_param_property(param) {
                    this.emitter.content.p_newline();
                    this.emitter.content.p("this");
                    this.emitter.content.p_dot();
                    this.visit_binding(param.name);
                    this.emitter.content.p_whitespace();
                    this.emitter.content.p_eq();
                    this.emitter.content.p_whitespace();
                    this.visit_binding(param.name);
                }
            },
            |this, param| {
                if this.is_param_property(param) {
                    this.emitter.content.p_newline();
                }
            },
        );

        self.emit_stmts_skip_omitted(after_stmts);

        if has_block_stmt {
            self.emitter.print().p_newline();
        }
        self.emitter.print().indent -= self.emitter.options.indent;
        self.emitter.print().p_r_brace();
    }

    fn visit_class_static_block_decl(
        &mut self,
        node: &'cx ast::ClassStaticBlockDecl<'cx>,
    ) -> Self::Result {
        self.emitter.print().p("static");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_brace();
        self.visit_block_stmt(node.body);
        self.emitter.print().p_r_brace();
    }

    fn visit_class_prop_elem(&mut self, node: &'cx ast::ClassPropElem<'cx>) -> Self::Result {
        if let Some(mods) = node.modifiers
            && mods.flags.contains(ast::ModifierFlags::ABSTRACT)
        {
            return;
        }
        self.emit_static_modifier(node.modifiers);
        self.visit_prop_name(node.name);
        if let Some(init) = node.init {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.visit_expr(init);
        }
        self.emitter.print().p_semi();
    }

    fn visit_class_method_elem(&mut self, node: &'cx ast::ClassMethodElem<'cx>) -> Self::Result {
        let Some(body) = node.body else {
            return;
        };
        self.emit_static_modifier(node.modifiers);
        if node.asterisk.is_some() {
            self.emitter.print().p_asterisk();
        }
        self.visit_prop_name(node.name);
        self.emit_params_without_this(node.params);
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(body);
    }

    fn visit_class_elem(&mut self, node: &'cx ast::ClassElem<'cx>) -> Self::Result {
        use ast::ClassElemKind::*;
        match node.kind {
            Prop(n) => self.visit_class_prop_elem(n),
            Method(n) => self.visit_class_method_elem(n),
            StaticBlockDecl(n) => self.visit_class_static_block_decl(n),
            Ctor(n) => self.visit_class_ctor(n),
            Getter(n) => self.visit_getter_decl(n),
            Setter(n) => self.visit_setter_decl(n),
            IndexSig(_) | Semi(_) => {}
        }
    }

    fn visit_fn_decl(&mut self, node: &'cx ast::FnDecl<'cx>) -> Self::Result {
        let Some(body) = node.body else {
            return;
        };
        if let Some(name) = node.name {
            self.ns_names.insert((self.scope, name.name));
        }
        self.emit_export_modifier_if_root(node.modifiers);
        if node
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::ASYNC))
        {
            self.emitter.print().p("async");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("function");
        if node.asterisk.is_some() {
            self.emitter.print().p("*");
        }
        self.emitter.print().p_whitespace();
        if let Some(name) = node.name {
            self.visit_ident(name);
        }
        self.emit_params_without_this(node.params);
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(body);
    }

    fn visit_class_decl(&mut self, node: &'cx ast::ClassDecl<'cx>) -> Self::Result {
        if node
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
        {
            return;
        }
        self.emit_export_modifier_if_root(node.modifiers);
        self.emitter.print().p("class");
        self.emitter.print().p_whitespace();
        if let Some(name) = node.name {
            self.visit_ident(name);
            self.ns_names.insert((self.scope, name.name));
            self.emitter.print().p_whitespace();
        }
        if let Some(extends) = node.extends {
            self.emit_class_extends_clause(extends);
        }
        self.emit_class_body(node.elems);
    }

    fn visit_class_expr(&mut self, node: &'cx ast::ClassExpr<'cx>) -> Self::Result {
        self.emitter.print().p("class");
        self.emitter.print().p_whitespace();
        if let Some(name) = node.name {
            self.visit_ident(name);
            self.ns_names.insert((self.scope, name.name));
            self.emitter.print().p_whitespace();
        }
        if let Some(extends) = node.extends {
            self.emit_class_extends_clause(extends);
        }
        self.emit_class_body(node.elems);
    }

    fn visit_throw_stmt(&mut self, node: &'cx ast::ThrowStmt<'cx>) -> Self::Result {
        self.emitter.print().p("throw");
        self.emitter.print().p_whitespace();
        self.visit_expr(node.expr);
    }

    fn visit_block_module_decl(&mut self, node: &'cx ast::BlockModuleDecl<'cx>) -> Self::Result {
        self.emit_block_module_decl(node);
    }

    fn visit_nested_module_decl(&mut self, node: &'cx ast::NestedModuleDecl<'cx>) -> Self::Result {
        if node
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
        {
            return;
        }
        match node.block {
            ast::NestedModuleBlock::Nested(inner) => {
                if !self.nested_module_instantiated(node) {
                    return;
                }
                let param_name = self.atoms().get(node.name.name).to_string();
                self.emit_with_var_fn_wrapper(node.name, &param_name, |this| {
                    this.emitter.content.p_newline();
                    this.visit_nested_module_decl(inner);
                    this.emitter.content.p_newline();
                    if let Some(ms) = inner.modifiers
                        && ms.flags.contains(ast::ModifierFlags::EXPORT)
                        && !ms.flags.contains(ast::ModifierFlags::AMBIENT)
                    {
                        this.emitter.content.p(&param_name);
                        this.emitter.content.p_dot();
                        this.visit_ident(inner.name);
                        this.emitter.content.p_whitespace();
                        this.emitter.content.p_eq();
                        this.emitter.content.p_whitespace();
                        this.visit_ident(inner.name);
                        this.emitter.content.p_semi();
                        this.emitter.content.p_newline();
                    }
                });
            }
            ast::NestedModuleBlock::Block(block) => {
                self.emit_block_module_decl(&ast::BlockModuleDecl {
                    id: node.id,
                    span: node.span,
                    modifiers: node.modifiers,
                    is_global_argument: false,
                    name: ast::ModuleName::Ident(node.name),
                    block: Some(block),
                });
            }
        }
    }

    fn visit_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) -> Self::Result {
        if node
            .modifiers
            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
        {
            return;
        }
        self.emit_with_var_fn_wrapper(node.name, self.atoms().get(node.name.name), |this| {
            for member in node.members {
                this.emitter.content.p_newline();
                this.visit_ident(node.name);
                this.emitter.content.p_l_bracket();
                this.visit_ident(node.name);
                this.emitter.content.p_l_bracket();
                this.emit_enum_member_name(&member.name);
                this.emitter.content.p_r_bracket();
                this.emitter.content.p_whitespace();
                this.emitter.content.p_eq();
                this.emitter.content.p_whitespace();
                if let Some(init) = member.init {
                    this.visit_expr(init);
                } else {
                    this.emitter.content.p("0");
                }
                this.emitter.content.p_r_bracket();
                this.emitter.content.p_whitespace();
                this.emitter.content.p_eq();
                this.emitter.content.p_whitespace();
                this.emit_enum_member_name(&member.name);
            }
        });
    }

    fn visit_enum_member(&mut self, _node: &'cx ast::EnumMember<'cx>) -> Self::Result {
        // Enum members are emitted inline by visit_enum_decl.
    }

    fn visit_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) -> Self::Result {
        self.emitter.print().p("import");
        self.emitter.print().p_whitespace();
        if let Some(clause) = node.clause {
            self.visit_import_clause(clause);
        }
        self.emitter.print().p_whitespace();
        self.emitter.print().p("from");
        self.emitter.print().p_whitespace();
        self.visit_string_lit(node.module);
    }

    fn visit_import_clause(&mut self, node: &'cx ast::ImportClause<'cx>) -> Self::Result {
        if let Some(name) = node.name {
            self.visit_ident(name);
            self.emitter.print().p_whitespace();
        } else if let Some(kind) = node.kind {
            match kind {
                ast::ImportClauseKind::Specs(specs) => {
                    self.emit_list(
                        specs,
                        |this, spec| this.visit_import_spec(spec),
                        |this, _| {
                            this.emitter.content.p_comma();
                            this.emitter.content.p_whitespace();
                        },
                    );
                }
                ast::ImportClauseKind::Ns(ns) => self.visit_ns_import(ns),
            }
        }
    }

    fn visit_ns_import(&mut self, node: &'cx ast::NsImport<'cx>) -> Self::Result {
        self.emitter.print().p_asterisk();
        self.emitter.print().p_whitespace();
        self.emitter.print().p("as");
        self.emitter.print().p_whitespace();
        self.visit_ident(node.name);
    }

    fn visit_module_export_name(&mut self, node: &'cx ast::ModuleExportName<'cx>) -> Self::Result {
        match node.kind {
            ast::ModuleExportNameKind::Ident(ident) => self.visit_ident(ident),
            ast::ModuleExportNameKind::StringLit(lit) => self.visit_string_lit(lit),
        }
    }

    fn visit_import_shorthand_spec(
        &mut self,
        node: &'cx ast::ImportShorthandSpec<'cx>,
    ) -> Self::Result {
        self.visit_ident(node.name);
    }

    fn visit_import_named_spec(&mut self, node: &'cx ast::ImportNamedSpec<'cx>) -> Self::Result {
        self.visit_module_export_name(node.prop_name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p("as");
        self.emitter.print().p_whitespace();
        self.visit_ident(node.name);
    }

    fn visit_stmt(&mut self, node: &'cx ast::Stmt<'cx>) -> Self::Result {
        use ast::StmtKind::*;
        match node.kind {
            Var(n) => self.visit_var_stmt(n),
            Expr(n) => {
                self.visit_expr_stmt(n);
                self.emitter.print().p_semi();
            }
            Fn(n) => self.visit_fn_decl(n),
            If(n) => self.visit_if_stmt(n),
            Block(n) => self.visit_block_stmt(n),
            Ret(n) => {
                self.visit_ret_stmt(n);
                self.emitter.print().p_semi();
            }
            Class(n) => self.visit_class_decl(n),
            Throw(n) => self.visit_throw_stmt(n),
            NestedModule(n) => self.visit_nested_module_decl(n),
            BlockModule(n) => self.visit_block_module_decl(n),
            Enum(n) => self.visit_enum_decl(n),
            Import(n) => self.visit_import_decl(n),
            ImportEquals(n) => self.visit_import_equals_decl(n),
            Export(n) => self.visit_export_decl(n),
            ExportAssign(n) => {
                self.visit_export_assign(n);
                self.emitter.print().p_semi();
            }
            For(n) => self.visit_for_stmt(n),
            ForOf(n) => self.visit_for_of_stmt(n),
            ForIn(n) => self.visit_for_in_stmt(n),
            Break(n) => {
                self.visit_break_stmt(n);
                self.emitter.print().p_semi();
            }
            Continue(n) => {
                self.visit_continue_stmt(n);
                self.emitter.print().p_semi();
            }
            Try(n) => self.visit_try_stmt(n),
            While(n) => self.visit_while_stmt(n),
            Do(n) => self.visit_do_while_stmt(n),
            Labeled(n) => self.visit_labeled_stmt(n),
            Empty(_) => self.emitter.print().p_semi(),
            Switch(n) => self.visit_switch_stmt(n),
            Interface(_) | TypeAlias(_) | Debugger(_) => {}
        }
    }

    fn visit_switch_stmt(&mut self, node: &'cx ast::SwitchStmt<'cx>) -> Self::Result {
        self.emitter.print().p("switch");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_paren();
        self.visit_expr(node.expr);
        self.emitter.print().p_r_paren();
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_brace();
        self.visit_case_block(node.case_block);
        self.emitter.print().p_r_brace();
    }

    fn visit_case_block(&mut self, node: &'cx ast::CaseBlock<'cx>) -> Self::Result {
        if !node.clauses.is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_list(
            node.clauses,
            |this, item| match item {
                ast::CaseOrDefaultClause::Case(n) => this.visit_case_clause(n),
                ast::CaseOrDefaultClause::Default(n) => this.visit_default_clause(n),
            },
            |this, _| {
                this.emitter.content.p_newline();
            },
        );
        if !node.clauses.is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
    }

    fn visit_case_clause(&mut self, node: &'cx ast::CaseClause<'cx>) -> Self::Result {
        self.emitter.print().p("case");
        self.emitter.print().p_whitespace();
        self.visit_expr(node.expr);
        self.emitter.print().p_colon();
        let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
        if has_stmts {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_stmts_skip_omitted(node.stmts);
        if has_stmts {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
    }

    fn visit_default_clause(&mut self, node: &'cx ast::DefaultClause<'cx>) -> Self::Result {
        self.emitter.print().p("default");
        self.emitter.print().p_colon();
        let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
        if has_stmts {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_stmts_skip_omitted(node.stmts);
        if has_stmts {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
    }

    fn visit_export_assign(&mut self, node: &'cx ast::ExportAssign<'cx>) -> Self::Result {
        self.emitter.print().p("export default");
        self.emitter.print().p_whitespace();
        self.visit_expr(node.expr);
    }

    fn visit_labeled_stmt(&mut self, node: &'cx ast::LabeledStmt<'cx>) -> Self::Result {
        self.visit_ident(node.label);
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.visit_stmt(node.stmt);
    }

    fn visit_do_while_stmt(&mut self, node: &'cx ast::DoWhileStmt<'cx>) -> Self::Result {
        self.emitter.print().p("do");
        self.emitter.print().p_whitespace();
        self.visit_stmt(node.stmt);
        self.emitter.print().p_whitespace();
        self.emitter.print().p("while");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_paren();
        self.visit_expr(node.expr);
        self.emitter.print().p_r_paren();
    }

    fn visit_while_stmt(&mut self, node: &'cx ast::WhileStmt<'cx>) -> Self::Result {
        self.emitter.print().p("while");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_paren();
        self.visit_expr(node.expr);
        self.emitter.print().p_r_paren();
        self.emitter.print().p_whitespace();
        self.visit_stmt(node.stmt);
    }

    fn visit_catch_clause(&mut self, node: &'cx ast::CatchClause<'cx>) -> Self::Result {
        self.emitter.print().p("catch");
        self.emitter.print().p_whitespace();
        if let Some(var) = node.var {
            self.emitter.print().p("(");
            self.visit_var_decl(var);
            self.emitter.print().p(")");
        }
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(node.block);
    }

    fn visit_try_stmt(&mut self, node: &'cx ast::TryStmt<'cx>) -> Self::Result {
        self.emitter.print().p("try");
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(node.try_block);
        if let Some(catch) = node.catch_clause {
            self.emitter.print().p_whitespace();
            self.visit_catch_clause(catch);
        }
        if let Some(finally) = node.finally_block {
            self.emitter.print().p("finally");
            self.emitter.print().p_whitespace();
            self.visit_block_stmt(finally);
        }
    }

    fn visit_continue_stmt(&mut self, node: &'cx ast::ContinueStmt<'cx>) -> Self::Result {
        self.emitter.print().p("continue");
        if let Some(label) = node.label {
            self.emitter.print().p_whitespace();
            self.visit_ident(label);
        }
    }

    fn visit_break_stmt(&mut self, node: &'cx ast::BreakStmt<'cx>) -> Self::Result {
        self.emitter.print().p("break");
        if let Some(label) = node.label {
            self.emitter.print().p_whitespace();
            self.visit_ident(label);
        }
    }

    fn visit_for_in_stmt(&mut self, node: &'cx ast::ForInStmt<'cx>) -> Self::Result {
        self.emitter.print().p("for");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("(");
        self.emitter.print().p_whitespace();
        self.emit_for_init(node.init);
        self.emitter.print().p_whitespace();
        self.emitter.print().p("in");
        self.emitter.print().p_whitespace();
        self.visit_expr(node.expr);
        self.emitter.print().p(")");
        self.emitter.print().p_whitespace();
        self.visit_stmt(node.body);
    }

    fn visit_for_stmt(&mut self, node: &'cx ast::ForStmt<'cx>) -> Self::Result {
        self.emitter.print().p("for");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("(");
        self.emitter.print().p_whitespace();
        if let Some(init) = node.init {
            self.emit_for_init(init);
        }
        self.emitter.print().p_semi();
        self.emitter.print().p_whitespace();
        if let Some(cond) = node.cond {
            self.visit_expr(cond);
        }
        self.emitter.print().p_semi();
        self.emitter.print().p_whitespace();
        if let Some(incr) = node.incr {
            self.visit_expr(incr);
        }
        self.emitter.print().p(")");
        self.emitter.print().p_whitespace();
        self.visit_stmt(node.body);
    }

    fn visit_for_of_stmt(&mut self, node: &'cx ast::ForOfStmt<'cx>) -> Self::Result {
        self.emitter.print().p("for");
        self.emitter.print().p_whitespace();
        if node.r#await.is_some() {
            self.emitter.print().p("await");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("(");
        self.emitter.print().p_whitespace();
        self.emit_for_init(node.init);
        self.emitter.print().p_whitespace();
        self.emitter.print().p("of");
        self.emitter.print().p_whitespace();
        self.visit_expr(node.expr);
        self.emitter.print().p(")");
        self.emitter.print().p_whitespace();
        self.visit_stmt(node.body);
    }

    fn visit_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) -> Self::Result {
        self.emitter.print().p("export");
        self.emitter.print().p_whitespace();
        match node.clause.kind {
            ast::ExportClauseKind::Specs(specs) => {
                self.emitter.print().p("{");
                self.emitter.print().p_whitespace();
                self.emit_list(
                    specs.list,
                    |this, spec| this.visit_export_spec(spec),
                    |this, _| {
                        this.emitter.content.p_comma();
                        this.emitter.content.p_whitespace();
                    },
                );
                self.emitter.print().p_whitespace();
                self.emitter.print().p("}");
                if let Some(module) = specs.module {
                    self.emitter.print().p_whitespace();
                    self.emitter.print().p("from");
                    self.emitter.print().p_whitespace();
                    self.visit_string_lit(module);
                }
            }
            ast::ExportClauseKind::Ns(n) => self.visit_ns_export(n),
            ast::ExportClauseKind::Glob(n) => {
                self.emitter.print().p("*");
                self.emitter.print().p_whitespace();
                self.emitter.print().p("from");
                self.emitter.print().p_whitespace();
                self.visit_string_lit(n.module);
            }
        }
    }

    fn visit_export_named_spec(&mut self, node: &'cx ast::ExportNamedSpec<'cx>) -> Self::Result {
        self.visit_module_export_name(node.prop_name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p("as");
        self.emitter.print().p_whitespace();
        self.visit_module_export_name(node.name);
    }

    fn visit_export_shorthand_spec(
        &mut self,
        node: &'cx ast::ExportShorthandSpec<'cx>,
    ) -> Self::Result {
        self.visit_ident(node.name);
    }

    fn visit_ns_export(&mut self, node: &'cx ast::NsExport<'cx>) -> Self::Result {
        self.emitter.print().p("*");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("as");
        self.emitter.print().p_whitespace();
        self.visit_module_export_name(node.name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p("from");
        self.emitter.print().p_whitespace();
        self.visit_string_lit(node.module);
    }

    fn visit_assign_expr(&mut self, node: &'cx ast::AssignExpr<'cx>) -> Self::Result {
        self.visit_expr(node.left);
        self.emitter.print().p_whitespace();
        self.emitter.print().p(node.op.as_str());
        self.emitter.print().p_whitespace();
        self.visit_expr(node.right);
    }

    fn visit_bin_expr(&mut self, node: &'cx ast::BinExpr<'cx>) -> Self::Result {
        self.visit_expr(node.left);
        self.emitter.print().p_whitespace();
        self.emitter.print().p(node.op.kind.as_str());
        self.emitter.print().p_whitespace();
        self.visit_expr(node.right);
    }

    fn visit_paren_expr(&mut self, node: &'cx ast::ParenExpr<'cx>) -> Self::Result {
        self.emitter.print().p_l_paren();
        self.visit_expr(node.expr);
        self.emitter.print().p_r_paren();
    }

    fn visit_this_expr(&mut self, _node: &'cx ast::ThisExpr) -> Self::Result {
        self.emitter.print().p("this");
    }

    fn visit_big_int_lit(&mut self, node: &'cx ast::BigIntLit) -> Self::Result {
        if node.val.0 {
            self.emitter.print().p("-");
        }
        let content = self.atoms().get(node.val.1);
        self.emitter.print().p(content);
        self.emitter.print().p("n");
    }

    fn visit_reg_exp_lit(&mut self, node: &'cx ast::RegExpLit) -> Self::Result {
        let content = self.atoms().get(node.val);
        self.emitter.print().p(content);
    }

    fn visit_array_lit(&mut self, node: &'cx ast::ArrayLit<'cx>) -> Self::Result {
        self.emitter.print().p_l_bracket();
        for (idx, expr) in node.elems.iter().enumerate() {
            self.visit_expr(expr);
            if idx != node.elems.len() - 1 {
                self.emitter.print().p_comma();
                self.emitter.print().p_whitespace();
            }
        }
        self.emitter.print().p_r_bracket();
    }

    fn visit_object_lit(&mut self, node: &'cx ast::ObjectLit<'cx>) -> Self::Result {
        if node.members.is_empty() {
            self.emitter.print().p("{}");
            return;
        }
        self.emitter.print().p_l_brace();
        self.emitter.print().p_newline();
        self.emitter.print().indent += self.emitter.options.indent;
        let indent = self.emitter.print().indent;
        self.emitter.print().p_pieces_of_whitespace(indent);
        self.emit_list(
            node.members,
            |this, member| {
                this.visit_object_member(member);
            },
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_newline();
            },
        );
        let indent = self.emitter.print().indent;
        self.emitter.print().p_pieces_of_whitespace(indent);
        self.emitter.print().indent -= self.emitter.options.indent;
        self.emitter.print().p_newline();
        self.emitter.print().p_r_brace();
    }

    fn visit_object_method_member(
        &mut self,
        node: &'cx ast::ObjectMethodMember<'cx>,
    ) -> Self::Result {
        if node.asterisk.is_some() {
            self.emitter.print().p_asterisk();
        }
        self.visit_prop_name(node.name);
        self.emit_params_without_this(node.params);
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(node.body);
    }

    fn visit_object_prop_assignment(
        &mut self,
        node: &'cx ast::ObjectPropAssignment<'cx>,
    ) -> Self::Result {
        self.visit_prop_name(node.name);
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.visit_expr(node.init);
    }

    fn visit_object_shorthand_member(
        &mut self,
        node: &'cx ast::ObjectShorthandMember<'cx>,
    ) -> Self::Result {
        self.visit_ident(node.name);
    }

    fn visit_prop_access_expr(&mut self, node: &'cx ast::PropAccessExpr<'cx>) -> Self::Result {
        if let ast::ExprKind::NumLit(n) = node.expr.kind {
            self.visit_num_lit(n);
            if n.val.fract() == 0. {
                self.emitter.print().p(".");
            }
        } else {
            self.visit_expr(node.expr);
        }
        self.emitter.print().p_dot();
        self.visit_ident(node.name);
    }

    fn visit_template_expr(&mut self, node: &'cx ast::TemplateExpr<'cx>) -> Self::Result {
        self.emitter.print().p("`");
        let content = self.atoms().get(node.head.text);
        let content = escape_snippet_text(content);
        self.emitter.print().p(&content);
        for span in node.spans {
            self.emitter.print().p("${");
            self.visit_expr(span.expr);
            self.emitter.print().p("}");
            let content = self.atoms().get(span.text);
            self.emitter.print().p(content);
        }
        self.emitter.print().p("`");
    }

    fn visit_no_substitution_template_lit(
        &mut self,
        node: &'cx ast::NoSubstitutionTemplateLit,
    ) -> Self::Result {
        let content = get_source_text_from_source(&self.origin, node.span);
        self.emitter.print().p(content);
    }

    fn visit_expr(&mut self, node: &'cx ast::Expr<'cx>) -> Self::Result {
        use ast::ExprKind::*;
        match node.kind {
            Assign(n) => self.visit_assign_expr(n),
            Bin(n) => self.visit_bin_expr(n),
            Omit(_) => {}
            Paren(n) => self.visit_paren_expr(n),
            This(n) => self.visit_this_expr(n),
            Ident(n) => self.visit_ident(n),
            BoolLit(n) => self.emitter.print().p(&n.val.to_string()),
            NullLit(_) => self.emitter.print().p("null"),
            NumLit(n) => self.visit_num_lit(n),
            BigIntLit(n) => self.visit_big_int_lit(n),
            RegExpLit(n) => self.visit_reg_exp_lit(n),
            StringLit(n) => self.visit_string_lit(n),
            NoSubstitutionTemplateLit(n) => self.visit_no_substitution_template_lit(n),
            ArrayLit(n) => self.visit_array_lit(n),
            ObjectLit(n) => self.visit_object_lit(n),
            Void(n) => {
                self.emitter.print().p("void");
                self.emitter.print().p_whitespace();
                self.visit_expr(n.expr);
            }
            Typeof(n) => {
                self.emitter.print().p("typeof");
                self.emitter.print().p_whitespace();
                self.visit_expr(n.expr);
            }
            Super(_) => self.emitter.print().p("super"),
            EleAccess(n) => {
                self.visit_expr(n.expr);
                self.emitter.print().p_l_bracket();
                self.visit_expr(n.arg);
                self.emitter.print().p_r_bracket();
            }
            PropAccess(n) => self.visit_prop_access_expr(n),
            PostfixUnary(n) => {
                self.visit_expr(n.expr);
                self.emitter.print().p(n.op.as_str());
            }
            PrefixUnary(n) => {
                self.emitter.print().p(n.op.as_str());
                if matches!(n.expr.kind, PrefixUnary(_)) {
                    self.emitter.print().p_whitespace();
                }
                self.visit_expr(n.expr);
            }
            TaggedTemplate(n) => {
                self.visit_expr(n.tag);
                match n.tpl {
                    ast::TemplateExpressionKind::NoSubstitutionTemplateLit(n) => {
                        self.visit_no_substitution_template_lit(n);
                    }
                    ast::TemplateExpressionKind::TemplateExpr(n) => {
                        self.visit_template_expr(n);
                    }
                }
            }
            Template(n) => self.visit_template_expr(n),
            SpreadElement(n) => {
                self.emitter.print().p("...");
                self.visit_expr(n.expr);
            }
            ArrowFn(n) => self.visit_arrow_fn_expr(n),
            New(n) => self.visit_new_expr(n),
            Class(n) => self.visit_class_expr(n),
            Fn(n) => self.visit_fn_expr(n),
            Call(n) => self.visit_call_expr(n),
            Cond(n) => self.visit_cond_expr(n),
            JsxElem(n) => self.visit_jsx_elem(n),
            JsxSelfClosingElem(n) => self.visit_jsx_self_closing_elem(n),
            JsxFrag(n) => self.visit_jsx_frag(n),
            Delete(n) => {
                self.emitter.print().p("delete");
                self.emitter.print().p_whitespace();
                self.visit_expr(n.expr);
            }
            Await(n) => {
                self.emitter.print().p("await");
                self.emitter.print().p_whitespace();
                self.visit_expr(n.expr);
            }
            Yield(n) => {
                self.emitter.print().p("yield");
                self.emitter.print().p_whitespace();
                if n.asterisk.is_some() {
                    self.emitter.print().p_asterisk();
                    self.emitter.print().p_whitespace();
                }
                if let Some(expr) = n.expr {
                    self.visit_expr(expr);
                }
            }
            NewMetaProperty(n) => {
                self.emitter.print().p("new");
                self.emitter.print().p(".");
                self.visit_ident(n.name);
            }
            As(n) => self.visit_expr(n.expr),
            Satisfies(n) => self.visit_expr(n.expr),
            NonNull(n) => self.visit_expr(n.expr),
            TyAssertion(n) => {
                if matches!(n.expr.kind, ast::ExprKind::ObjectLit(_)) {
                    self.emitter.print().p_l_paren();
                    self.visit_expr(n.expr);
                    self.emitter.print().p_r_paren();
                } else {
                    self.visit_expr(n.expr);
                }
            }
            ExprWithTyArgs(n) => self.visit_expr_with_ty_args(n),
            Import(_) => todo!(),
        }
    }

    fn visit_jsx_ns_name(&mut self, node: &'cx ast::JsxNsName<'cx>) -> Self::Result {
        self.visit_ident(node.ns);
        self.emitter.print().p(":");
        self.visit_ident(node.name);
    }

    fn visit_jsx_expr(&mut self, node: &'cx ast::JsxExpr<'cx>) -> Self::Result {
        self.emitter.print().p_l_brace();
        if node.dotdotdot_token.is_some() {
            self.emitter.print().p_dot_dot_dot();
        }
        if let Some(expr) = node.expr {
            self.visit_expr(expr);
        }
        self.emitter.print().p_r_brace();
    }

    fn visit_jsx_frag(&mut self, node: &'cx ast::JsxFrag<'cx>) -> Self::Result {
        self.emitter.print().p("<>");
        for child in node.children {
            self.visit_jsx_child(*child);
        }
        self.emitter.print().p("</>");
    }

    fn visit_jsx_self_closing_elem(
        &mut self,
        node: &'cx ast::JsxSelfClosingElem<'cx>,
    ) -> Self::Result {
        self.emitter.print().p("<");
        self.visit_jsx_tag_name(node.tag_name);
        self.emitter.print().p_whitespace();
        for attr in node.attrs {
            self.emitter.print().p_whitespace();
            self.visit_jsx_attr(attr);
        }
        self.emitter.print().p(" />");
    }

    fn visit_jsx_elem(&mut self, node: &'cx ast::JsxElem<'cx>) -> Self::Result {
        self.emitter.print().p("<");
        self.visit_jsx_tag_name(node.opening_elem.tag_name);
        self.emitter.print().p_whitespace();
        for attr in node.opening_elem.attrs {
            self.emitter.print().p_whitespace();
            self.visit_jsx_attr(attr);
        }
        self.emitter.print().p(">");

        for child in node.children {
            self.visit_jsx_child(*child);
        }

        self.emitter.print().p("</");
        self.visit_jsx_tag_name(node.closing_elem.tag_name);
        self.emitter.print().p(">");
    }

    fn visit_cond_expr(&mut self, node: &'cx ast::CondExpr<'cx>) -> Self::Result {
        self.visit_expr(node.cond);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_question();
        self.emitter.print().p_whitespace();
        self.visit_expr(node.when_true);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.visit_expr(node.when_false);
    }

    fn visit_fn_expr(&mut self, node: &'cx ast::FnExpr<'cx>) -> Self::Result {
        self.emitter.print().p("function");
        if node.asterisk.is_some() {
            self.emitter.print().p("*");
        }
        self.emitter.print().p_whitespace();
        if let Some(name) = node.name {
            self.visit_ident(name);
        }
        self.emit_params_without_this(node.params);
        self.emitter.print().p_whitespace();
        self.visit_block_stmt(node.body);
    }

    fn visit_call_expr(&mut self, node: &'cx ast::CallExpr<'cx>) -> Self::Result {
        self.visit_expr(node.expr);
        self.emit_args(node.args);
    }

    fn visit_new_expr(&mut self, node: &'cx ast::NewExpr<'cx>) -> Self::Result {
        self.emitter.print().p("new");
        self.emitter.print().p_whitespace();
        self.visit_expr(node.expr);
        match node.args {
            Some(args) => self.emit_args(args),
            None => self.emit_args(&[]),
        }
    }

    fn visit_expr_with_ty_args(&mut self, node: &'cx ast::ExprWithTyArgs<'cx>) -> Self::Result {
        self.visit_expr(node.expr);
    }

    fn visit_arrow_fn_expr(&mut self, node: &'cx ast::ArrowFnExpr<'cx>) -> Self::Result {
        if node.async_modifier.is_some() {
            self.emitter.print().p("async");
            self.emitter.print().p_whitespace();
        }
        self.emit_params_without_this(node.params);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_arrow_right();
        self.emitter.print().p_whitespace();

        match node.body {
            ast::ArrowFnExprBody::Expr(expr) => {
                self.emitter.print().p("(");
                self.visit_expr(expr);
                self.emitter.print().p(")");
            }
            ast::ArrowFnExprBody::Block(block) => {
                let mut non_omitted = block.stmts.iter().filter(|s| !self.stmt_is_omitted(s));
                let single = non_omitted.next().filter(|_| non_omitted.next().is_none());
                if let Some(stmt) = single
                    && let ast::StmtKind::Ret(ret) = stmt.kind
                    && let Some(expr) = ret.expr
                {
                    self.emitter.print().p("(");
                    self.visit_expr(expr);
                    self.emitter.print().p(")");
                } else {
                    self.visit_block_stmt(block);
                }
            }
        }
    }

    fn visit_import_equals_decl(&mut self, node: &'cx ast::ImportEqualsDecl<'cx>) -> Self::Result {
        if self.resolver.is_import_equals_namespace_module(node) {
            return;
        }
        self.emitter.print().p("var");
        self.emitter.print().p_whitespace();
        self.visit_ident(node.name);
        self.emitter.print().p_whitespace();
        self.emitter.print().p_eq();
        self.emitter.print().p_whitespace();
        match node.module_reference {
            ast::ModuleReferenceKind::EntityName(n) => self.visit_entity_name(n),
            ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                self.visit_string_lit(n.module_spec())
            }
        }
    }

    fn visit_qualified_name(&mut self, node: &'cx ast::QualifiedName<'cx>) -> Self::Result {
        self.visit_entity_name(node.left);
        self.emitter.print().p_dot();
        self.visit_ident(node.right);
    }

    fn visit_entity_name(&mut self, node: &'cx ast::EntityName<'cx>) -> Self::Result {
        use ast::EntityNameKind::*;
        match &node.kind {
            Ident(n) => self.visit_ident(n),
            Qualified(n) => self.visit_qualified_name(n),
        }
    }

    noop_visit_type_node!();
}

fn get_source_text_from_source(source: &str, span: bolt_ts_span::Span) -> &str {
    let lo = span.lo() as usize;
    let hi = span.hi() as usize;
    debug_assert!(lo < hi);
    &source[lo..hi]
}

fn escape_snippet_text(s: &str) -> String {
    s.replace('$', "\\$")
}
