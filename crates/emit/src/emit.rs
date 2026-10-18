use std::borrow::Cow;

use bolt_ts_ast::TokenKind;
use bolt_ts_ast::{self as ast};
use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_checker::{check::TyChecker, emit_resolver::EmitResolver};
use bolt_ts_scanner::is_line_break;
use bolt_ts_span::ModuleID;
use rustc_hash::FxHashSet;

use super::print::PPrint;

#[derive(Clone, Copy)]
pub struct EmitterOptions {
    indent: u32,
}

bolt_ts_utils::index! {
    ScopeID
}

pub fn emit_modules<'cx>(
    entries: Vec<ModuleID>,
    checker: &mut TyChecker<'cx>,
) -> Vec<(ModuleID, String, String)> {
    entries
        .into_iter()
        .filter_map(|item| {
            let is_default_lib = checker.module_arena.get_module(item).is_default_lib();
            if is_default_lib {
                None
            } else {
                let (js, dts) = emit_module(item, checker);
                Some((item, js, dts))
            }
        })
        .collect::<Vec<_>>()
}

fn emit_module<'cx>(module_id: ModuleID, checker: &mut TyChecker<'cx>) -> (String, String) {
    let options = checker.config.compiler_options();
    debug_assert!(!options.no_emit());
    let declaration = options.declaration();
    let emit_declaration_only = options.emit_declaration_only();
    let target = match (declaration, emit_declaration_only) {
        (true | false, true) => EmitTarget::DECLARATION,
        (true, false) => EmitTarget::all(),
        (false, false) => EmitTarget::JS,
    };
    let mut flags = EmitDeclarationFlags::empty();
    if declaration {
        flags = EmitDeclarationFlags::NEED_DECLARE;
        if checker.p.get(module_id).is_external_or_commonjs_module() {
            flags.remove(EmitDeclarationFlags::NEED_DECLARE);
        }
    }
    let resolver = EmitResolver::new(checker);
    // TODO: remove `to_string`.
    let origin = resolver.module_content(module_id).to_string();
    let mut emitter = ModuleEmitter {
        js: Emitter::new(),
        dts: Emitter::new(),
        resolver,
        ns_names: FxHashSet::default(),
        scope: ScopeID::root(),
        max_scope: ScopeID::root().next(),
        origin,
        module_id,
        comment_idx: 0,
        flags,
        target,
    };
    let root = emitter.resolver.program(module_id);
    emitter.emit_program(root);
    let js = emitter.js.print().take_content();
    let dts = if declaration {
        emitter.dts.print().take_content()
    } else {
        String::new()
    };
    (js, dts)
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct EmitDeclarationFlags: u8 {
        const STRIP_EXPORT_MODIFIER = 1 << 0;
        const NEED_DECLARE          = 1 << 1;
    }
}

fn contain_export_modifier<'cx>(flags: Option<&'cx ast::Modifiers<'cx>>) -> bool {
    flags.is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::EXPORT))
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

bitflags::bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq)]
    struct EmitTarget: u8 {
        const JS          = 0b01;
        const DECLARATION = 0b10;
    }
}

struct ModuleEmitter<'cx, 'a> {
    js: Emitter,
    dts: Emitter,
    resolver: EmitResolver<'cx, 'a>,
    ns_names: FxHashSet<(ScopeID, bolt_ts_atom::Atom)>,
    scope: ScopeID,
    max_scope: ScopeID,
    origin: String,
    module_id: ModuleID,
    comment_idx: usize,
    flags: EmitDeclarationFlags,
    target: EmitTarget,
}

impl<'cx, 'a> ModuleEmitter<'cx, 'a> {
    fn with_target(&mut self, target: EmitTarget, f: impl FnOnce(&mut Self)) {
        if self.target == EmitTarget::JS && target == EmitTarget::DECLARATION {
            return;
        }
        let saved = std::mem::replace(&mut self.target, target);
        f(self);
        self.target = saved;
    }

    fn dts(&mut self) -> Option<&mut Emitter> {
        if self.target == EmitTarget::all() {
            Some(&mut self.dts)
        } else {
            None
        }
    }

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
            |this, decl| this.emit_var_decl(decl),
            |this, _| {
                this.emit_token(TokenKind::Comma);
                this.js.content.p_whitespace();
            },
        );
    }

    fn emit_as_string(&mut self, val: Atom) {
        let s = self.atoms().get(val);
        self.emit_token_value(TokenKind::String, s);
    }

    fn emit_params(&mut self, params: ast::ParamsDecl<'cx>) {
        self.emit_token(TokenKind::LParen);
        self.emit_list(
            params,
            |this, item| this.emit_param_decl(item),
            |this, _| {
                this.emit_token(TokenKind::Comma);
                this.js.content.p_whitespace();
            },
        );
        self.emit_token(TokenKind::RParen);
    }

    fn emit_args(&mut self, args: ast::Exprs<'cx>) {
        self.emit_token(TokenKind::LParen);
        self.emit_list(
            args,
            |this, arg| this.emit_expr(arg),
            |this, _| {
                this.emit_token(TokenKind::Comma);
                this.js.content.p_whitespace();
            },
        );
        self.emit_token(TokenKind::RParen);
    }

    fn emit_export_modifier_if_root(&mut self, modifiers: Option<&'cx ast::Modifiers<'cx>>) {
        if let Some(ms) = modifiers {
            if self.scope == ScopeID::root() && ms.flags.contains(ast::ModifierFlags::EXPORT) {
                self.emit_token(TokenKind::Export);
                self.js.print().p_whitespace();
            }
            if self.scope == ScopeID::root() && ms.flags.contains(ast::ModifierFlags::DEFAULT) {
                self.emit_token(TokenKind::Default);
                self.js.print().p_whitespace();
            }
        }
    }

    fn emit_static_modifier(&mut self, modifiers: Option<&'cx ast::Modifiers<'cx>>) {
        if let Some(ms) = modifiers
            && ms.flags.contains(ast::ModifierFlags::STATIC)
        {
            self.emit_token(TokenKind::Static);
            self.js.print().p_whitespace();
        }
    }

    fn emit_class_extends_clause_js(&mut self, extends: &'cx ast::ClassExtendsClause<'cx>) {
        self.emit_token(TokenKind::Extends);
        self.js.print().p_whitespace();
        self.emit_expr_with_ty_args(extends.expr_with_ty_args);
        self.js.print().p_whitespace();
    }

    fn emit_with_var_fn_wrapper(
        &mut self,
        decl_name: &'cx ast::Ident,
        param_name: &str,
        f: impl FnOnce(&mut Self),
    ) {
        let name = decl_name.name;
        if self.ns_names.insert((self.scope, name)) {
            self.emit_token(TokenKind::Var);
            self.js.print().p_whitespace();
            self.emit_ident(decl_name);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Eq);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LBrace);
            self.emit_token(TokenKind::RBrace);
            self.emit_token(TokenKind::Semi);
        }

        self.js.print().p_newline();

        self.emit_token(TokenKind::LParen);
        self.emit_token(TokenKind::Function);
        self.js.print().p_whitespace();
        self.emit_token(TokenKind::LParen);

        self.js.print().p(param_name);
        self.emit_token(TokenKind::RParen);
        self.js.print().p_whitespace();

        // emit block
        self.emit_token(TokenKind::LBrace);
        self.js.print().p_newline();
        self.js.print().indent += self.js.options.indent;
        let old = self.scope;
        self.scope = self.next_scope();

        f(self);

        self.scope = old;
        self.js.print().indent -= self.js.options.indent;
        self.js.print().p_newline();
        self.emit_token(TokenKind::RBrace);

        self.emit_token(TokenKind::RParen);
        self.emit_token(TokenKind::LParen);
        self.emit_ident(decl_name);
        self.emit_token(TokenKind::RParen);
        self.emit_token(TokenKind::Semi);
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
                self.sync_comment_idx(stmt.span().hi());
                continue;
            }
            self.js.content.p_newline();
            self.emit_stmt(stmt);
            self.js.content.p_newline();
            let t = match stmt.kind {
                ast::StmtKind::Var(v) => {
                    if let Some(ms) = v.modifiers
                        && ms.flags.contains(ast::ModifierFlags::EXPORT)
                        && !ms.flags.contains(ast::ModifierFlags::AMBIENT)
                    {
                        for item in v.list {
                            self.js.content.p(param_name);
                            self.emit_token(TokenKind::Dot);
                            self.emit_binding(item.name);
                            self.js.content.p_whitespace();
                            self.emit_token(TokenKind::Eq);
                            self.js.content.p_whitespace();
                            self.emit_binding(item.name);
                            self.js.content.p_newline();
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
                self.js.content.p(param_name);
                self.emit_token(TokenKind::Dot);
                self.emit_ident(name);
                self.js.content.p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.content.p_whitespace();
                self.emit_ident(name);
                self.emit_token(TokenKind::Semi);
                self.js.content.p_newline();
            }
        }
    }

    // TODO: remove this
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
                self.sync_comment_idx(stmt.span().hi());
                if self.target == EmitTarget::all() {
                    self.with_target(EmitTarget::DECLARATION, |this| this.emit_stmt(stmt));
                }
                continue;
            }
            if !first {
                self.js.content.p_newline();
            }
            self.emit_stmt(stmt);
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
        self.emit_token(TokenKind::LParen);
        let mut params = params;
        if let Some((first, rest)) = params.split_first()
            && self.is_this_param(first)
        {
            let pos = rest.first().map_or(first.span.hi(), |p| p.span.lo());
            self.sync_comment_idx(pos);
            params = rest;
        }
        self.emit_list(
            params,
            |this, item| this.emit_param_decl(item),
            |this, _| {
                this.emit_token(TokenKind::Comma);
                this.js.content.p_whitespace();
            },
        );
        self.emit_token(TokenKind::RParen);
    }

    // dts gets the old `emit_parameters` form (with parens/commas when
    // `dts_parens`) or the bare walk form (without).
    fn emit_fn_params(&mut self, params: ast::ParamsDecl<'cx>, dts_parens: bool) {
        self.emit_token(TokenKind::LParen);
        if dts_parens && let Some(dts) = self.dts() {
            dts.print().p_token(TokenKind::LParen);
        }
        let mut params = params;
        let mut dts_first = true;
        if let Some((first, rest)) = params.split_first()
            && self.is_this_param(first)
        {
            let pos = rest.first().map_or(first.span.hi(), |p| p.span.lo());
            self.sync_comment_idx(pos);
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_param_decl(first));
            dts_first = false;
            params = rest;
        }
        let mut js_first = true;
        for param in params {
            if !js_first {
                self.emit_token(TokenKind::Comma);
                self.js.content.p_whitespace();
            }
            if dts_parens && !dts_first {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Comma);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
            }
            self.emit_param_decl(param);
            js_first = false;
            dts_first = false;
        }
        self.emit_token(TokenKind::RParen);
        if dts_parens && let Some(dts) = self.dts() {
            dts.print().p_token(TokenKind::RParen);
        }
    }

    fn emit_class_body(&mut self, elems: &'cx ast::ClassElems<'cx>, dts_braces: bool) {
        let has_items = elems.list.iter().any(|e| !self.class_elem_is_empty(e));
        self.emit_token(TokenKind::LBrace);
        let dts_on = self.target == EmitTarget::all();
        if dts_on
            && dts_braces
            && let Some(dts) = self.dts()
        {
            dts.print().p_token(TokenKind::LBrace);
        }
        if has_items {
            self.js.print().indent += self.js.options.indent;
            self.js.print().p_newline();
        }
        let dts_wrap = dts_on && dts_braces && !elems.list.is_empty();
        if dts_wrap {
            if let Some(dts) = self.dts() {
                dts.increment_indent();
            }
            if let Some(dts) = self.dts() {
                dts.print().p_newline();
            }
        }
        let mut first = true;
        let mut dts_first = true;
        for elem in elems.list {
            if self.class_elem_is_empty(elem) {
                let end = match elem.kind {
                    ast::ClassElemKind::Ctor(n) => n.span.hi(),
                    ast::ClassElemKind::Prop(n) => n.span.hi(),
                    ast::ClassElemKind::Method(n) => n.span.hi(),
                    ast::ClassElemKind::IndexSig(n) => n.span.hi(),
                    ast::ClassElemKind::Getter(n) => n.span.hi(),
                    ast::ClassElemKind::Setter(n) => n.span.hi(),
                    ast::ClassElemKind::StaticBlockDecl(n) => n.span.hi(),
                    ast::ClassElemKind::Semi(n) => n.span.hi(),
                };
                if dts_on {
                    if matches!(
                        elem.kind,
                        ast::ClassElemKind::IndexSig(_) | ast::ClassElemKind::Semi(_)
                    ) {
                        self.sync_comment_idx(end);
                    }
                } else {
                    self.sync_comment_idx(end);
                    continue;
                }
            } else {
                if !first {
                    self.js.content.p_newline();
                }
                first = false;
            }
            if dts_on {
                if dts_braces
                    && !dts_first
                    && let Some(dts) = self.dts()
                {
                    dts.print().p_newline();
                }
                dts_first = false;
            }
            self.emit_class_elem(elem);
        }
        if has_items {
            self.js.print().indent -= self.js.options.indent;
            self.js.print().p_newline();
        }
        if dts_wrap {
            if let Some(dts) = self.dts() {
                dts.decrement_indent();
            }
            if let Some(dts) = self.dts() {
                dts.print().p_newline();
            }
        }
        self.emit_token(TokenKind::RBrace);
        if dts_on
            && dts_braces
            && let Some(dts) = self.dts()
        {
            dts.print().p_token(TokenKind::RBrace);
        }
    }

    fn emit_enum_member_name(&mut self, name: &'cx ast::EnumMemberNameKind<'cx>) {
        use ast::EnumMemberNameKind::*;
        match name {
            Ident(ident) => self.emit_as_string(ident.name),
            StringLit { raw, .. } => self.emit_string_lit(raw),
        }
    }

    fn emit_import_spec(&mut self, node: &'cx ast::ImportSpec<'cx>) {
        match node.kind {
            ast::ImportSpecKind::Shorthand(n) => self.emit_import_shorthand_spec(n),
            ast::ImportSpecKind::Named(n) => self.emit_import_named_spec(n),
        }
    }

    fn emit_for_init(&mut self, init: ast::ForInitKind<'cx>) {
        match init {
            ast::ForInitKind::Var(decls) => {
                self.emit_token(TokenKind::Var);
                self.js.print().p_whitespace();
                self.emit_var_decls(decls);
            }
            ast::ForInitKind::Expr(expr) => self.emit_expr(expr),
        }
    }

    fn emit_export_spec(&mut self, node: &'cx ast::ExportSpec<'cx>) {
        match node.kind {
            ast::ExportSpecKind::Shorthand(n) => self.emit_export_shorthand_spec(n),
            ast::ExportSpecKind::Named(n) => self.emit_export_named_spec(n),
        }
    }

    fn emit_object_member(&mut self, node: &'cx ast::ObjectMember<'cx>) {
        use ast::ObjectMemberKind::*;
        match node.kind {
            PropAssignment(n) => self.emit_object_prop_assignment(n),
            Shorthand(n) => self.emit_object_shorthand_member(n),
            Method(n) => self.emit_object_method_member(n),
            SpreadAssignment(n) => {
                self.emit_token(TokenKind::DotDotDot);
                self.emit_expr(n.expr);
            }
            Getter(n) => self.emit_getter_decl(n),
            Setter(n) => self.emit_setter_decl(n),
        }
    }

    fn emit_jsx_tag_name(&mut self, node: ast::JsxTagName<'cx>) {
        use ast::JsxTagName::*;
        match node {
            Ident(n) => self.emit_ident(n),
            Ns(n) => self.emit_jsx_ns_name(n),
            PropAccess(n) => self.emit_prop_access_expr(n),
            This(_) => self.emit_token(TokenKind::This),
        }
    }

    fn emit_jsx_attr(&mut self, node: &'cx ast::JsxAttr<'cx>) {
        match node {
            ast::JsxAttr::Spread(n) => {
                self.emit_token(TokenKind::LBrace);
                self.emit_token(TokenKind::DotDotDot);
                self.emit_expr(n.expr);
                self.emit_token(TokenKind::RBrace);
            }
            ast::JsxAttr::Named(n) => {
                self.emit_jsx_attr_name(n.name.clone());
                if let Some(v) = n.init {
                    self.emit_token(TokenKind::Eq);
                    self.emit_jsx_attr_value(v);
                }
            }
        }
    }

    fn emit_jsx_attr_name(&mut self, node: ast::JsxAttrName<'cx>) {
        use ast::JsxAttrName::*;
        match node {
            Ident(n) => self.emit_ident(n),
            Ns(n) => self.emit_jsx_ns_name(n),
        }
    }

    fn emit_jsx_attr_value(&mut self, node: ast::JsxAttrValue<'cx>) {
        use ast::JsxAttrValue::*;
        match node {
            StringLit(n) => self.emit_string_lit(n),
            Expr(n) => self.emit_jsx_expr(n),
            Ele(n) => self.emit_jsx_elem(n),
            SelfClosingEle(n) => self.emit_jsx_self_closing_elem(n),
            Frag(n) => self.emit_jsx_frag(n),
        }
    }

    fn emit_jsx_child(&mut self, child: ast::JsxChild<'cx>) {
        use ast::JsxChild::*;
        match child {
            Text(n) => {
                let content = self.atoms().get(n.text);
                self.js.print().p(content);
            }
            Expr(n) => self.emit_jsx_expr(n),
            Elem(n) => self.emit_jsx_elem(n),
            SelfClosingEle(n) => self.emit_jsx_self_closing_elem(n),
            Frag(n) => self.emit_jsx_frag(n),
        }
    }

    fn emit_instantiated_block_module_decl(&mut self, node: &ast::BlockModuleDecl<'cx>) {
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

    /// TODO: `sync_comment_idx` is generate by LLM and I don't like this method,
    /// maybe a better way is count the token for pointed node, and then skip it.
    fn sync_comment_idx(&mut self, pos: u32) {
        let comments = self.resolver.get_leading_trailing_comments(self.module_id);
        while let Some((&p, at_token)) = comments.get_comments_by_index(self.comment_idx) {
            if p >= pos {
                break;
            }
            for c in at_token
                .get_leading_comments()
                .iter()
                .chain(at_token.get_trailing_comments())
            {
                let text = &self.origin[c.start() as usize..c.end() as usize];
                let text = text.replace("\r\n", "\n");
                self.js.content.p(&text);
                self.js.content.p_newline();
            }
            self.comment_idx += 1;
        }
    }

    fn emit_token(&mut self, token: TokenKind) {
        self.emit_token_value(token, token.as_str());
    }

    fn emit_token_value(&mut self, token: TokenKind, val: &str) {
        if self.resolver.config().compiler_options().remove_comments() {
            print_token_value(&mut self.js, token, val);
            return;
        }
        // emit token with comments
        let comments = self.resolver.get_leading_trailing_comments(self.module_id);
        match comments.get_comments_by_index(self.comment_idx) {
            Some((&pos, comments)) => {
                let line_map = self.resolver.get_line_map(self.module_id);
                let pos = bolt_ts_parser::compute_line_and_char_of_pos(line_map, pos as usize);
                for c in comments.get_leading_comments() {
                    let text = &self.origin[c.start() as usize..c.end() as usize];
                    let text = text.replace("\r\n", "\n");
                    self.js.content.p(&text);
                    let comment_pos =
                        bolt_ts_parser::compute_line_and_char_of_pos(line_map, c.end() as usize);
                    if comment_pos.line == pos.line {
                        self.js.content.p_whitespace();
                    } else {
                        self.js.content.p_newline();
                    }
                }

                print_token_value(&mut self.js, token, val);

                for c in comments.get_trailing_comments() {
                    let text = &self.origin[c.start() as usize..c.end() as usize];
                    let text = text.replace("\r\n", "\n");
                    self.js.content.p(&text);
                    if c.kind() == bolt_ts_scanner::CommentKind::SingleLine {
                        self.js.content.p_newline();
                    } else {
                        self.js.content.p_whitespace();
                    }
                }
            }
            _ => {
                print_token_value(&mut self.js, token, val);
            }
        }
        self.comment_idx += 1;
    }

    fn emit_list_indexed<T>(
        &mut self,
        list: &[T],
        emit_item: impl Fn(&mut Self, &T, usize),
        emit_sep: impl Fn(&mut Self, &T, usize),
    ) {
        for (idx, item) in list.iter().enumerate() {
            emit_item(self, item, idx);
            if idx != list.len() - 1 {
                emit_sep(self, item, idx);
            }
        }
    }

    fn emit_ret_ty(&mut self, ty: Option<&'cx ast::Ty<'cx>>) {
        match ty {
            Some(ty) => {
                self.emit_ty(ty);
            }
            None => self.dts.print().p("any"),
        }
    }

    fn emit_type_parameters(&mut self, n: Option<ast::TyParams<'cx>>) {
        if let Some(n) = n {
            debug_assert!(!n.is_empty());
            self.dts.print().p_token(TokenKind::Less);
            self.emit_list_indexed(
                n,
                |this, item, _| {
                    this.emit_ident(item.name);
                    if let Some(constraint) = item.constraint {
                        this.dts.print().p_whitespace();
                        this.dts.print().p_token(TokenKind::Extends);
                        this.dts.print().p_whitespace();
                        this.emit_ty(constraint);
                    }
                    if let Some(default) = item.default {
                        this.dts.print().p_whitespace();
                        this.dts.print().p_token(TokenKind::Eq);
                        this.dts.print().p_whitespace();
                        this.emit_ty(default);
                    }
                },
                |this, _, _| {
                    this.dts.print().p_token(TokenKind::Comma);
                    this.dts.print().p_whitespace();
                },
            );
            self.dts.print().p_token(TokenKind::Great);
        }
    }

    fn emit_type_arguments(&mut self, n: Option<&'cx ast::Tys<'cx>>) {
        if let Some(n) = n {
            debug_assert!(!n.list.is_empty());
            self.dts.print().p_token(TokenKind::Less);
            self.emit_list_indexed(
                n.list,
                |this, item, _| {
                    this.emit_ty(item);
                },
                |this, _, _| {
                    this.dts.print().p_token(TokenKind::Comma);
                    this.dts.print().p_whitespace();
                },
            );
            self.dts.print().p_token(TokenKind::Great);
        }
    }

    fn emit_string_literal(&mut self, atom: bolt_ts_atom::Atom) {
        let atom = self.resolver.atoms().get(atom);
        self.dts.print().p_string_literal(atom);
    }

    fn emit_default_modifier(&mut self, ms: Option<&'cx ast::Modifiers<'cx>>) {
        if let Some(ms) = ms
            && ms.flags.contains(ast::ModifierFlags::DEFAULT)
        {
            self.dts.print().p_token(TokenKind::Default);
            self.dts.print().p_whitespace();
        }
    }

    fn emit_export_modifier_if_needed(&mut self, contain_export_modifier: bool) {
        if contain_export_modifier
            && !self
                .flags
                .contains(EmitDeclarationFlags::STRIP_EXPORT_MODIFIER)
        {
            self.dts.print().p_token(TokenKind::Export);
            self.dts.print().p_whitespace();
        }
    }

    fn emit_declare_if_needed(&mut self) {
        if self.flags.contains(EmitDeclarationFlags::NEED_DECLARE) {
            self.dts.print().p_token(TokenKind::Declare);
            self.dts.print().p_whitespace();
        }
    }

    fn emit_identifier_name_in_variable_declaration(
        &mut self,
        node: &'cx ast::Ident,
        binding: ast::NodeID,
    ) {
        self.dts.emit_atom(self.resolver.atoms(), node.name);
        self.dts.print().p_token(TokenKind::Colon);
        self.dts.print().p_whitespace();
        let ty = self.resolver.ensure_type_for_identifier_in_binding(binding);
        let ty_str = self.resolver.print_type(ty);
        self.dts.print().p(&ty_str);
    }

    fn emit_variable_declaration_by_array_pat(&mut self, node: &'cx ast::ArrayPat<'cx>) {
        let elements = node.elems.iter().filter(|element| match element.kind {
            ast::ArrayBindingElemKind::Omit(_) => false,
            ast::ArrayBindingElemKind::Binding(_) => true,
        });

        for (idx, element) in elements.enumerate() {
            if idx != 0 {
                self.dts.print().p_token(TokenKind::Comma);
                self.dts.print().p_whitespace();
            }
            if let ast::ArrayBindingElemKind::Binding(item) = &element.kind {
                self.emit_variable_by_binding(item.name, item.id);
            }
        }
    }

    fn emit_variable_declaration_by_object_pat(&mut self, node: &'cx ast::ObjectPat<'cx>) {
        self.emit_list_indexed(
            node.elems,
            |this, item, _| match item.name {
                ast::ObjectBindingName::Shorthand(ident) => {
                    this.emit_identifier_name_in_variable_declaration(ident, item.id);
                }
                ast::ObjectBindingName::Prop { name, .. } => {
                    this.emit_variable_by_binding(name, item.id);
                }
            },
            |this, _, _| {
                this.dts.print().p_token(TokenKind::Comma);
                this.dts.print().p_whitespace();
            },
        );
    }

    fn emit_variable_by_binding(&mut self, node: &'cx ast::Binding<'cx>, binding: ast::NodeID) {
        match node.kind {
            ast::BindingKind::Ident(name) => {
                self.emit_identifier_name_in_variable_declaration(name, binding);
            }
            ast::BindingKind::ArrayPat(node) => {
                self.emit_variable_declaration_by_array_pat(node);
            }
            ast::BindingKind::ObjectPat(node) => {
                self.emit_variable_declaration_by_object_pat(node);
            }
        }
    }

    fn emit_parameters(&mut self, params: ast::ParamsDecl<'cx>) {
        self.emit_list_indexed(
            params,
            |this, item, _| {
                this.emit_param_decl(item);
            },
            |this, _, _| {
                this.dts.print().p_token(TokenKind::Comma);
                this.dts.print().p_whitespace();
            },
        );
    }

    fn has_name_visible_for_binding(&self, node: &'cx ast::Binding<'cx>) -> bool {
        match node.kind {
            ast::BindingKind::Ident(_) => {
                // TODO: self.resolver.is_declaration_visible
                true
            }
            ast::BindingKind::ObjectPat(n) => self.has_name_visible_for_object_pat(n),
            ast::BindingKind::ArrayPat(n) => self.has_name_visible_for_array_pat(n),
        }
    }

    fn has_name_visible_for_object_pat(&self, node: &'cx ast::ObjectPat<'cx>) -> bool {
        node.elems.iter().any(|element| {
            match element.name {
                ast::ObjectBindingName::Shorthand(_) => {
                    // TODO: self.resolver.is_declaration_visible
                    true
                }
                ast::ObjectBindingName::Prop { name, .. } => {
                    self.has_name_visible_for_binding(name)
                }
            }
        })
    }

    fn has_name_visible_for_array_pat(&self, node: &'cx ast::ArrayPat<'cx>) -> bool {
        node.elems.iter().any(|element| match element.kind {
            ast::ArrayBindingElemKind::Omit(_) => false,
            ast::ArrayBindingElemKind::Binding(item) => {
                self.has_name_visible_for_binding(item.name)
            }
        })
    }
    fn emit_program(&mut self, node: &'cx ast::Program<'cx>) {
        if self.target == EmitTarget::JS {
            let mut first = true;
            for stmt in node.stmts() {
                if self.stmt_is_omitted(stmt) {
                    self.sync_comment_idx(stmt.span().hi());
                    continue;
                }
                if !first {
                    self.js.content.p_newline();
                }
                self.emit_stmt(stmt);
                first = false;
            }
        } else if self.target == EmitTarget::DECLARATION {
            for stmt in node.stmts() {
                self.emit_stmt(stmt);
                self.dts.print().p_newline();
            }
        } else {
            let mut first = true;
            for stmt in node.stmts() {
                if self.stmt_is_omitted(stmt) {
                    self.sync_comment_idx(stmt.span().hi());
                    self.emit_stmt(stmt);
                    if let Some(dts) = self.dts() {
                        dts.print().p_newline();
                    }
                    continue;
                }
                if !first {
                    self.js.content.p_newline();
                }
                self.emit_stmt(stmt);
                if let Some(dts) = self.dts() {
                    dts.print().p_newline();
                }
                first = false;
            }
        }
    }
    fn emit_var_stmt(&mut self, node: &'cx ast::VarStmt<'cx>) {
        if self.target == EmitTarget::JS {
            if node
                .modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
            {
                return;
            }
            self.emit_token(TokenKind::Var);
            self.js.print().p_whitespace();
            self.emit_var_decls(node.list);
            if next_adjacent_token_on_this_line_is_semi(
                self.origin.as_bytes(),
                node.span.hi() as usize - 1,
            ) {
                self.emit_token(TokenKind::Semi);
            } else {
                self.js.print().p_token(TokenKind::Semi);
            }
        } else if self.target == EmitTarget::DECLARATION {
            if !node
                .list
                .iter()
                .all(|n| self.has_name_visible_for_binding(n.name))
            {
                return;
            }

            self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));

            self.emit_declare_if_needed();

            let node_flags = self.resolver.node_flags(node.id);
            if node_flags.contains(ast::NodeFlags::CONST) {
                self.dts.print().p_token(TokenKind::Const);
            } else if node_flags.contains(ast::NodeFlags::LET) {
                self.dts.print().p_token(TokenKind::Let);
            } else {
                self.dts.print().p_token(TokenKind::Var);
            }
            self.dts.print().p_whitespace();

            self.emit_list_indexed(
                node.list,
                |this, item, _| {
                    this.emit_var_decl(item);
                },
                |this, _, _| {
                    this.dts.print().p_token(TokenKind::Comma);
                    this.dts.print().p_whitespace();
                },
            );
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            let js_on = !node
                .modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT));
            let dts_on = self.target == EmitTarget::all()
                && node
                    .list
                    .iter()
                    .all(|n| self.has_name_visible_for_binding(n.name));
            match (js_on, dts_on) {
                (true, true) => {
                    self.emit_token(TokenKind::Var);
                    self.js.print().p_whitespace();
                    self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
                    self.emit_declare_if_needed();
                    let node_flags = self.resolver.node_flags(node.id);
                    if node_flags.contains(ast::NodeFlags::CONST) {
                        if let Some(dts) = self.dts() {
                            dts.print().p_token(TokenKind::Const);
                        }
                    } else if node_flags.contains(ast::NodeFlags::LET) {
                        if let Some(dts) = self.dts() {
                            dts.print().p_token(TokenKind::Let);
                        }
                    } else {
                        if let Some(dts) = self.dts() {
                            dts.print().p_token(TokenKind::Var);
                        }
                    }
                    if let Some(dts) = self.dts() {
                        dts.print().p_whitespace();
                    }
                    for (idx, item) in node.list.iter().enumerate() {
                        if idx != 0 {
                            self.emit_token(TokenKind::Comma);
                            self.js.content.p_whitespace();
                            if let Some(dts) = self.dts() {
                                dts.print().p_token(TokenKind::Comma);
                            }
                            if let Some(dts) = self.dts() {
                                dts.print().p_whitespace();
                            }
                        }
                        self.emit_var_decl(item);
                    }
                    if next_adjacent_token_on_this_line_is_semi(
                        self.origin.as_bytes(),
                        node.span.hi() as usize - 1,
                    ) {
                        self.emit_token(TokenKind::Semi);
                    } else {
                        self.js.print().p_token(TokenKind::Semi);
                    }
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::Semi);
                    }
                }
                (true, false) => self.with_target(EmitTarget::JS, |this| this.emit_var_stmt(node)),
                (false, true) => {
                    self.with_target(EmitTarget::DECLARATION, |this| this.emit_var_stmt(node))
                }
                (false, false) => {}
            }
        }
    }
    fn emit_var_decl(&mut self, node: &'cx ast::VarDecl<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_binding(node.name);
            if let Some(excl) = node.excl {
                self.sync_comment_idx(excl.hi());
            }
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.emit_expr(init);
            }
        } else if self.target == EmitTarget::DECLARATION {
            match node.name.kind {
                ast::BindingKind::Ident(name) => {
                    self.dts.emit_atom(self.resolver.atoms(), name.name);
                    self.dts.print().p_token(TokenKind::Colon);
                    self.dts.print().p_whitespace();
                    if let Some(ty) = node.ty {
                        self.emit_ty(ty);
                    } else {
                        let ty = self.resolver.ensure_type_for_variable_declaration(node);
                        let ty_str = self.resolver.print_type(ty);
                        self.dts.print().p(&ty_str);
                    }
                }
                ast::BindingKind::ArrayPat(n) => self.emit_variable_declaration_by_array_pat(n),
                ast::BindingKind::ObjectPat(n) => self.emit_variable_declaration_by_object_pat(n),
            }
        } else {
            self.target = EmitTarget::JS;
            self.emit_var_decl(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_var_decl(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_ident(&mut self, node: &'cx ast::Ident) {
        if self.target == EmitTarget::JS {
            let content = self.atoms().get(node.name);
            self.emit_token_value(TokenKind::Ident, content);
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.emit_atom(self.resolver.atoms(), node.name);
        } else {
            let content = self.atoms().get(node.name);
            self.emit_token_value(TokenKind::Ident, content);
            if self.target == EmitTarget::all() {
                self.dts.emit_atom(self.resolver.atoms(), node.name);
            }
        }
    }

    fn emit_private_ident(&mut self, node: &'cx ast::PrivateIdent) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_private_ident(node)
        } else {
            let content = self.atoms().get(node.name);
            self.emit_token_value(TokenKind::PrivateIdent, content);
        }
    }
    fn emit_num_lit(&mut self, node: &'cx ast::NumLit) {
        if self.target == EmitTarget::JS {
            self.emit_token_value(TokenKind::Number, &node.val.to_string())
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p(&node.val.to_string());
        } else {
            self.emit_token_value(TokenKind::Number, &node.val.to_string());
            if let Some(dts) = self.dts() {
                dts.print().p(&node.val.to_string());
            }
        }
    }
    fn emit_string_lit(&mut self, node: &'cx ast::StringLit) {
        if self.target == EmitTarget::JS {
            self.emit_as_string(node.val);
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_string_literal(node.val);
        } else {
            self.emit_as_string(node.val);
            if self.target == EmitTarget::all() {
                self.emit_string_literal(node.val);
            }
        }
    }
    fn emit_prop_name(&mut self, node: &'cx ast::PropName<'cx>) {
        if self.target == EmitTarget::JS {
            use ast::PropNameKind::*;
            match &node.kind {
                Ident(n) => self.emit_ident(n),
                NumLit(n) => self.emit_num_lit(n),
                StringLit { raw, .. } => self.emit_string_lit(raw),
                Computed(n) => {
                    self.emit_token(TokenKind::LBracket);
                    self.emit_expr(n.expr);
                    self.emit_token(TokenKind::RBracket);
                }
                PrivateIdent(n) => self.emit_private_ident(n),
                BigIntLit(n) => {
                    let content = self.atoms().get(n.val.1);
                    self.emit_token_value(TokenKind::BigInt, content);
                }
            }
        } else if self.target == EmitTarget::DECLARATION {
            use ast::PropNameKind::*;
            match &node.kind {
                Ident(n) => self.emit_ident(n),
                StringLit { raw, .. } => self.emit_string_lit(raw),
                BigIntLit { .. } => todo!(),
                NumLit(n) => self.emit_num_lit(n),
                Computed(n) => {
                    self.emit_computed_prop_name(n);
                }
                PrivateIdent(_private_ident) => todo!(),
            }
        } else {
            use ast::PropNameKind::*;
            match &node.kind {
                Ident(n) => self.emit_ident(n),
                NumLit(n) => self.emit_num_lit(n),
                StringLit { raw, .. } => self.emit_string_lit(raw),
                Computed(n) => {
                    self.emit_token(TokenKind::LBracket);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::LBracket);
                    }
                    self.emit_expr(n.expr);
                    self.emit_token(TokenKind::RBracket);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::RBracket);
                    }
                }
                PrivateIdent(n) => {
                    self.emit_private_ident(n);
                }
                BigIntLit(n) => {
                    let content = self.atoms().get(n.val.1);
                    self.emit_token_value(TokenKind::BigInt, content);
                }
            }
        }
    }
    fn emit_object_binding_elem(&mut self, node: &'cx ast::ObjectBindingElem<'cx>) {
        if self.target == EmitTarget::JS {
            if node.dotdotdot.is_some() {
                self.emit_token(TokenKind::DotDotDot);
            }
            match node.name {
                ast::ObjectBindingName::Shorthand(ident) => {
                    self.emit_ident(ident);
                }
                ast::ObjectBindingName::Prop { prop_name, name } => {
                    self.emit_prop_name(prop_name);
                    self.emit_token(TokenKind::Colon);
                    self.js.print().p_whitespace();
                    self.emit_binding(name);
                }
            }
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.emit_expr(init);
            }
        } else if self.target == EmitTarget::DECLARATION {
            if node.dotdotdot.is_some() {
                self.dts.print().p_token(TokenKind::DotDotDot);
            }
            match node.name {
                ast::ObjectBindingName::Shorthand(ident) => self.emit_ident(ident),
                ast::ObjectBindingName::Prop { prop_name, name } => {
                    self.emit_prop_name(prop_name);
                    self.dts.print().p_token(TokenKind::Colon);
                    self.dts.print().p_whitespace();
                    self.emit_binding(name);
                }
            }
        } else {
            if node.dotdotdot.is_some() {
                self.emit_token(TokenKind::DotDotDot);
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::DotDotDot);
                }
            }
            match node.name {
                ast::ObjectBindingName::Shorthand(ident) => {
                    self.emit_ident(ident);
                }
                ast::ObjectBindingName::Prop { prop_name, name } => {
                    self.emit_prop_name(prop_name);
                    self.emit_token(TokenKind::Colon);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::Colon);
                    }
                    self.js.print().p_whitespace();
                    if let Some(dts) = self.dts() {
                        dts.print().p_whitespace();
                    }
                    self.emit_binding(name);
                }
            }
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.with_target(EmitTarget::JS, |this| this.emit_expr(init));
            }
        }
    }
    fn emit_array_binding(&mut self, node: &'cx ast::ArrayBinding<'cx>) {
        if self.target == EmitTarget::JS {
            if node.dotdotdot.is_some() {
                self.emit_token(TokenKind::DotDotDot);
            }
            self.emit_binding(node.name);
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.emit_expr(init);
            }
        } else if self.target == EmitTarget::DECLARATION {
            if node.dotdotdot.is_some() {
                self.dts.print().p_token(TokenKind::DotDotDot);
            }
            self.emit_binding(node.name);
        } else {
            if node.dotdotdot.is_some() {
                self.emit_token(TokenKind::DotDotDot);
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::DotDotDot);
                }
            }
            self.emit_binding(node.name);
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.with_target(EmitTarget::JS, |this| this.emit_expr(init));
            }
        }
    }
    fn emit_binding(&mut self, node: &'cx ast::Binding<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_binding(node)
        } else if self.target == EmitTarget::JS {
            use ast::BindingKind::*;
            match node.kind {
                Ident(n) => self.emit_ident(n),
                ObjectPat(n) => {
                    self.emit_token(TokenKind::LBrace);
                    self.emit_list(
                        n.elems,
                        |this, item| this.emit_object_binding_elem(item),
                        |this, _| {
                            this.emit_token(TokenKind::Comma);
                            this.js.content.p_whitespace();
                        },
                    );
                    self.emit_token(TokenKind::RBrace);
                }
                ArrayPat(n) => {
                    self.emit_token(TokenKind::LBracket);
                    self.emit_list(
                        n.elems,
                        |this, item| match item.kind {
                            ast::ArrayBindingElemKind::Omit(_) => {}
                            ast::ArrayBindingElemKind::Binding(n) => {
                                this.emit_array_binding(n);
                            }
                        },
                        |this, _| {
                            this.emit_token(TokenKind::Comma);
                            this.js.content.p_whitespace();
                        },
                    );
                    self.emit_token(TokenKind::RBracket);
                }
            }
        } else {
            use ast::BindingKind::*;
            match node.kind {
                Ident(n) => self.emit_ident(n),
                ObjectPat(n) => {
                    self.emit_token(TokenKind::LBrace);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::LBrace);
                    }
                    for (idx, item) in n.elems.iter().enumerate() {
                        if idx != 0 {
                            self.emit_token(TokenKind::Comma);
                            self.js.content.p_whitespace();
                            if let Some(dts) = self.dts() {
                                dts.print().p_token(TokenKind::Comma);
                            }
                            if let Some(dts) = self.dts() {
                                dts.print().p_whitespace();
                            }
                        }
                        self.emit_object_binding_elem(item);
                    }
                    self.emit_token(TokenKind::RBrace);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::RBrace);
                    }
                }
                ArrayPat(n) => {
                    self.emit_token(TokenKind::LBracket);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::LBracket);
                    }
                    for (idx, item) in n.elems.iter().enumerate() {
                        if idx != 0 {
                            self.emit_token(TokenKind::Comma);
                            self.js.content.p_whitespace();
                            if let Some(dts) = self.dts() {
                                dts.print().p_token(TokenKind::Comma);
                            }
                            if let Some(dts) = self.dts() {
                                dts.print().p_whitespace();
                            }
                        }
                        match item.kind {
                            ast::ArrayBindingElemKind::Omit(_) => {}
                            ast::ArrayBindingElemKind::Binding(n) => {
                                self.emit_array_binding(n);
                            }
                        }
                    }
                    self.emit_token(TokenKind::RBracket);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::RBracket);
                    }
                }
            }
        }
    }
    fn emit_param_decl(&mut self, node: &'cx ast::ParamDecl<'cx>) {
        if self.target == EmitTarget::JS {
            if node.dotdotdot.is_some() {
                self.emit_token(TokenKind::DotDotDot);
            }
            if node.modifiers.is_some() {
                self.sync_comment_idx(node.name.span.lo());
            }
            self.emit_binding(node.name);
            if let Some(question) = node.question {
                self.sync_comment_idx(question.hi());
            }
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.emit_expr(init);
            }
        } else if self.target == EmitTarget::DECLARATION {
            let n = node;
            if n.dotdotdot.is_some() {
                self.dts.print().p_token(TokenKind::DotDotDot);
            }
            self.emit_binding(n.name);
            if self.resolver.is_optional_parameter(n) {
                self.dts.print().p_token(TokenKind::Question);
            }
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();

            if let Some(ty) = n.ty {
                self.emit_ty(ty);
            } else {
                let ty = self
                    .resolver
                    .ensure_type_for_parameter_declaration::<true>(n);
                let ty = self.resolver.print_type(ty);
                self.dts.print().p(&ty);
            }
        } else {
            if node.dotdotdot.is_some() {
                self.emit_token(TokenKind::DotDotDot);
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::DotDotDot);
                }
            }
            if node.modifiers.is_some() {
                self.sync_comment_idx(node.name.span.lo());
            }
            self.emit_binding(node.name);
            if let Some(question) = node.question {
                self.sync_comment_idx(question.hi());
            }
            if self.target == EmitTarget::all()
                && self.resolver.is_optional_parameter(node)
                && let Some(dts) = self.dts()
            {
                dts.print().p_token(TokenKind::Question);
            }
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            if self.target == EmitTarget::all() {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Colon);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                if let Some(ty) = node.ty {
                    self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
                } else {
                    let ty = self
                        .resolver
                        .ensure_type_for_parameter_declaration::<true>(node);
                    let ty = self.resolver.print_type(ty);
                    if let Some(dts) = self.dts() {
                        dts.print().p(&ty);
                    }
                }
            }
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.with_target(EmitTarget::JS, |this| this.emit_expr(init));
            }
        }
    }
    fn emit_block_stmt(&mut self, node: &'cx ast::BlockStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_block_stmt(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::LBrace);
            let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
            if has_stmts {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_stmts_skip_omitted(node.stmts);
            if has_stmts {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_token(TokenKind::RBrace);
        } else {
            self.emit_token(TokenKind::LBrace);
            let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
            if has_stmts {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_stmts_skip_omitted(node.stmts);
            if has_stmts {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_token(TokenKind::RBrace);
        }
    }
    fn emit_if_stmt(&mut self, node: &'cx ast::IfStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_if_stmt(node)
        } else {
            self.emit_token(TokenKind::If);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LParen);
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::RParen);
            self.js.print().p_whitespace();
            self.emit_stmt(node.then);
            if let Some(else_then) = node.else_then {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Else);
                self.js.print().p_whitespace();
                self.emit_stmt(else_then);
            }
            self.js.print().p_newline();
        }
    }
    fn emit_ret_stmt(&mut self, node: &'cx ast::RetStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_ret_stmt(node)
        } else {
            self.emit_token(TokenKind::Return);
            self.js.print().p_whitespace();
            if let Some(expr) = node.expr {
                self.emit_expr(expr);
            }
        }
    }
    fn emit_getter_decl(&mut self, node: &'cx ast::GetterDecl<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_getter_decl(node)
        } else if self.target == EmitTarget::JS {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                return;
            };
            self.emit_static_modifier(node.modifiers);
            self.emit_token(TokenKind::Get);
            self.js.print().p_whitespace();
            self.emit_prop_name(node.name);
            self.emit_params(&[]);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(body);
        } else {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                self.with_target(EmitTarget::DECLARATION, |this| {
                    this.emit_prop_name(node.name);
                    if let Some(ty) = node.ty {
                        this.emit_ty(ty);
                    }
                });
                return;
            };
            self.emit_static_modifier(node.modifiers);
            self.emit_token(TokenKind::Get);
            self.js.print().p_whitespace();
            self.emit_prop_name(node.name);
            self.emit_params(&[]);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(body);
        }
    }
    fn emit_setter_decl(&mut self, node: &'cx ast::SetterDecl<'cx>) {
        if self.target == EmitTarget::JS {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                return;
            };
            self.emit_static_modifier(node.modifiers);
            self.emit_token(TokenKind::Set);
            self.js.print().p_whitespace();
            self.emit_prop_name(node.name);
            self.emit_params_without_this(node.params);
            self.js.print().p_whitespace();
            self.emit_block_stmt(body);
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::Set);
            self.dts.print().p_whitespace();
            self.emit_prop_name(node.name);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);
        } else {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_setter_decl(node));
                return;
            };
            self.emit_static_modifier(node.modifiers);
            self.emit_token(TokenKind::Set);
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Set);
            }
            self.js.print().p_whitespace();
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            self.emit_prop_name(node.name);
            self.emit_fn_params(node.params, true);
            self.js.print().p_whitespace();
            self.with_target(EmitTarget::JS, |this| this.emit_block_stmt(body));
        }
    }
    fn emit_class_ctor(&mut self, node: &'cx ast::ClassCtor<'cx>) {
        if self.target == EmitTarget::JS {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                return;
            };
            self.emit_token(TokenKind::Constructor);
            self.emit_params_without_this(node.params);
            self.js.print().p_whitespace();

            self.emit_token(TokenKind::LBrace);
            self.js.print().indent += self.js.options.indent;

            let has_block_stmt = body.stmts.iter().any(|s| !self.stmt_is_omitted(s))
                && node
                    .params
                    .iter()
                    .any(|param| self.is_param_property(param));

            if has_block_stmt {
                self.js.print().p_newline();
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
                        this.js.content.p_newline();
                        this.emit_token(TokenKind::This);
                        this.emit_token(TokenKind::Dot);
                        this.emit_binding(param.name);
                        this.js.content.p_whitespace();
                        this.emit_token(TokenKind::Eq);
                        this.js.content.p_whitespace();
                        this.emit_binding(param.name);
                    }
                },
                |this, param| {
                    if this.is_param_property(param) {
                        this.js.content.p_newline();
                    }
                },
            );

            self.emit_stmts_skip_omitted(after_stmts);

            if has_block_stmt {
                self.js.print().p_newline();
            }
            self.js.print().indent -= self.js.options.indent;
            self.emit_token(TokenKind::RBrace);
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::Constructor);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);
            if let Some(ty) = node.ret {
                self.dts.print().p_token(TokenKind::Colon);
                self.dts.print().p_whitespace();
                self.emit_ty(ty);
            }
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_class_ctor(node));
                return;
            };
            self.emit_token(TokenKind::Constructor);
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Constructor);
            }
            self.emit_fn_params(node.params, true);
            if let Some(ty) = node.ret {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Colon);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
            }
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Semi);
            }
            self.js.print().p_whitespace();
            self.with_target(EmitTarget::JS, |this| {
                this.emit_token(TokenKind::LBrace);
                this.js.print().indent += this.js.options.indent;

                let has_block_stmt = body.stmts.iter().any(|s| !this.stmt_is_omitted(s))
                    && node
                        .params
                        .iter()
                        .any(|param| this.is_param_property(param));

                if has_block_stmt {
                    this.js.print().p_newline();
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

                this.emit_stmts_skip_omitted(prev_stmts);

                this.emit_list(
                    node.params,
                    |this, param| {
                        if this.is_param_property(param) {
                            this.js.content.p_newline();
                            this.emit_token(TokenKind::This);
                            this.emit_token(TokenKind::Dot);
                            this.emit_binding(param.name);
                            this.js.content.p_whitespace();
                            this.emit_token(TokenKind::Eq);
                            this.js.content.p_whitespace();
                            this.emit_binding(param.name);
                        }
                    },
                    |this, param| {
                        if this.is_param_property(param) {
                            this.js.content.p_newline();
                        }
                    },
                );

                this.emit_stmts_skip_omitted(after_stmts);

                if has_block_stmt {
                    this.js.print().p_newline();
                }
                this.js.print().indent -= this.js.options.indent;
                this.emit_token(TokenKind::RBrace);
            });
        }
    }
    fn emit_class_static_block_decl(&mut self, node: &'cx ast::ClassStaticBlockDecl<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_class_static_block_decl(node)
        } else {
            self.emit_token(TokenKind::Static);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LBrace);
            self.emit_block_stmt(node.body);
            self.emit_token(TokenKind::RBrace);
        }
    }
    fn emit_class_prop_elem(&mut self, node: &'cx ast::ClassPropElem<'cx>) {
        if self.target == EmitTarget::JS {
            if let Some(mods) = node.modifiers
                && mods.flags.contains(ast::ModifierFlags::ABSTRACT)
            {
                self.sync_comment_idx(node.span.hi());
                return;
            }
            self.emit_static_modifier(node.modifiers);
            if node.modifiers.is_some() {
                self.sync_comment_idx(node.name.span().lo());
            }
            self.emit_prop_name(node.name);
            if let Some(question) = node.question {
                self.sync_comment_idx(question.hi());
            }
            if let Some(excl) = node.excl {
                self.sync_comment_idx(excl.hi());
            }
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.emit_expr(init);
            }
            if next_adjacent_token_on_this_line_is_semi(
                self.origin.as_bytes(),
                node.span.hi() as usize - 1,
            ) {
                self.emit_token(TokenKind::Semi);
            } else {
                self.js.print().p_token(TokenKind::Semi);
            }
        } else if self.target == EmitTarget::DECLARATION {
            if let Some(ms) = node.modifiers
                && ms.flags.contains(ast::ModifierFlags::STATIC)
            {
                self.dts.print().p_token(TokenKind::Static);
                self.dts.print().p_whitespace();
            }
            self.emit_prop_name(node.name);
            if node.question.is_some() {
                self.dts.print().p_token(TokenKind::Question);
            }
            if let Some(ty) = node.ty {
                self.dts.print().p_token(TokenKind::Colon);
                self.dts.print().p_whitespace();
                self.emit_ty(ty);
            }
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            if let Some(mods) = node.modifiers
                && mods.flags.contains(ast::ModifierFlags::ABSTRACT)
            {
                self.sync_comment_idx(node.span.hi());
                self.with_target(EmitTarget::DECLARATION, |this| {
                    this.emit_class_prop_elem(node)
                });
                return;
            }
            self.emit_static_modifier(node.modifiers);
            if let Some(ms) = node.modifiers
                && ms.flags.contains(ast::ModifierFlags::STATIC)
            {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Static);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
            }
            if node.modifiers.is_some() {
                self.sync_comment_idx(node.name.span().lo());
            }
            self.emit_prop_name(node.name);
            if let Some(question) = node.question {
                self.sync_comment_idx(question.hi());
            }
            if node.question.is_some()
                && let Some(dts) = self.dts()
            {
                dts.print().p_token(TokenKind::Question);
            }
            if let Some(excl) = node.excl {
                self.sync_comment_idx(excl.hi());
            }
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            if let Some(ty) = node.ty {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Colon);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
            }
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Semi);
            }
            if let Some(init) = node.init {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                self.with_target(EmitTarget::JS, |this| this.emit_expr(init));
            }
            if next_adjacent_token_on_this_line_is_semi(
                self.origin.as_bytes(),
                node.span.hi() as usize - 1,
            ) {
                self.emit_token(TokenKind::Semi);
            } else {
                self.js.print().p_token(TokenKind::Semi);
            }
        }
    }
    fn emit_class_method_elem(&mut self, node: &'cx ast::ClassMethodElem<'cx>) {
        if self.target == EmitTarget::JS {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                return;
            };
            self.emit_static_modifier(node.modifiers);
            if node.asterisk.is_some() {
                self.emit_token(TokenKind::Asterisk);
            }
            self.emit_prop_name(node.name);
            self.emit_params_without_this(node.params);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(body);
        } else if self.target == EmitTarget::DECLARATION {
            if let Some(ms) = node.modifiers
                && ms.flags.contains(ast::ModifierFlags::STATIC)
            {
                self.dts.print().p_token(TokenKind::Static);
                self.dts.print().p_whitespace();
            }
            self.emit_prop_name(node.name);
            self.emit_type_parameters(node.ty_params);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            if let Some(ty) = node.ty {
                self.emit_ty(ty);
            } else {
                let ty = self.resolver.ensure_type_for_class_method_element(node);
                let ty_str = self.resolver.print_type(ty);
                self.dts.print().p(&ty_str);
            }
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
                self.with_target(EmitTarget::DECLARATION, |this| {
                    this.emit_class_method_elem(node)
                });
                return;
            };
            self.emit_static_modifier(node.modifiers);
            if let Some(ms) = node.modifiers
                && ms.flags.contains(ast::ModifierFlags::STATIC)
            {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Static);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
            }
            if node.asterisk.is_some() {
                self.emit_token(TokenKind::Asterisk);
            }
            self.emit_prop_name(node.name);
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_type_parameters(node.ty_params)
            });
            self.emit_fn_params(node.params, true);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Colon);
            }
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            if let Some(ty) = node.ty {
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
            } else if self.target == EmitTarget::all() {
                let ty = self.resolver.ensure_type_for_class_method_element(node);
                let ty_str = self.resolver.print_type(ty);
                if let Some(dts) = self.dts() {
                    dts.print().p(&ty_str);
                }
            }
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Semi);
            }
            self.js.print().p_whitespace();
            self.with_target(EmitTarget::JS, |this| this.emit_block_stmt(body));
        }
    }
    fn emit_class_elem(&mut self, node: &'cx ast::ClassElem<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_class_elem(node)
        } else if self.target == EmitTarget::JS {
            use ast::ClassElemKind::*;
            match node.kind {
                Prop(n) => self.emit_class_prop_elem(n),
                Method(n) => self.emit_class_method_elem(n),
                StaticBlockDecl(n) => self.emit_class_static_block_decl(n),
                Ctor(n) => self.emit_class_ctor(n),
                Getter(n) => self.emit_getter_decl(n),
                Setter(n) => self.emit_setter_decl(n),
                IndexSig(_) | Semi(_) => {}
            }
        } else {
            use ast::ClassElemKind::*;
            match node.kind {
                Prop(n) => self.emit_class_prop_elem(n),
                Method(n) => self.emit_class_method_elem(n),
                StaticBlockDecl(n) => self.emit_class_static_block_decl(n),
                Ctor(n) => self.emit_class_ctor(n),
                Getter(n) => self.emit_getter_decl(n),
                Setter(n) => self.emit_setter_decl(n),
                IndexSig(n) => {
                    self.with_target(EmitTarget::DECLARATION, |this| this.emit_index_sig_decl(n));
                }
                Semi(_) => {}
            }
        }
    }
    fn emit_fn_decl(&mut self, node: &'cx ast::FnDecl<'cx>) {
        if self.target == EmitTarget::JS {
            let Some(body) = node.body else {
                self.sync_comment_idx(node.span.hi());
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
                self.emit_token(TokenKind::Async);
                self.js.print().p_whitespace();
            }
            self.emit_token(TokenKind::Function);
            if node.asterisk.is_some() {
                self.emit_token(TokenKind::Asterisk);
            }
            self.js.print().p_whitespace();
            if let Some(name) = node.name {
                self.emit_ident(name);
            }
            self.emit_params_without_this(node.params);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(body);
        } else if self.target == EmitTarget::DECLARATION {
            let n = node;
            // TODO: top level
            if self
                .resolver
                .is_implementation_of_overload_for_function_declaration(n)
            {
                return;
            }
            self.emit_export_modifier_if_needed(contain_export_modifier(n.modifiers));
            self.emit_default_modifier(n.modifiers);
            self.emit_declare_if_needed();
            self.dts.print().p_token(TokenKind::Function);
            self.dts.print().p_whitespace();
            if let Some(name) = n.name.map(|name| name.name) {
                self.dts.emit_atom(self.resolver.atoms(), name);
            }
            self.emit_type_parameters(n.ty_params);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(n.params);
            self.dts.print().p_token(TokenKind::RParen);
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            if let Some(ty) = n.ty {
                self.emit_ty(ty);
            } else {
                let ty = self.resolver.ensure_type_for_function_declaration(n);
                let ty_str = self.resolver.print_type(ty);
                self.dts.print().p(&ty_str);
            }
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            let js_on = node.body.is_some();
            let dts_on = self.target == EmitTarget::all()
                && !self
                    .resolver
                    .is_implementation_of_overload_for_function_declaration(node);
            match (js_on, dts_on) {
                (true, true) => {
                    let body = node.body.unwrap();
                    if let Some(name) = node.name {
                        self.ns_names.insert((self.scope, name.name));
                    }
                    self.emit_export_modifier_if_root(node.modifiers);
                    self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
                    self.emit_default_modifier(node.modifiers);
                    self.emit_declare_if_needed();
                    if node
                        .modifiers
                        .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::ASYNC))
                    {
                        self.emit_token(TokenKind::Async);
                        self.js.print().p_whitespace();
                    }
                    self.emit_token(TokenKind::Function);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::Function);
                    }
                    if node.asterisk.is_some() {
                        self.emit_token(TokenKind::Asterisk);
                    }
                    self.js.print().p_whitespace();
                    if let Some(dts) = self.dts() {
                        dts.print().p_whitespace();
                    }
                    if let Some(name) = node.name {
                        self.emit_ident(name);
                    }
                    self.with_target(EmitTarget::DECLARATION, |this| {
                        this.emit_type_parameters(node.ty_params)
                    });
                    self.emit_fn_params(node.params, true);
                    if let Some(ty) = node.ty {
                        self.sync_comment_idx(ty.span().hi());
                    }
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::Colon);
                    }
                    if let Some(dts) = self.dts() {
                        dts.print().p_whitespace();
                    }
                    if let Some(ty) = node.ty {
                        self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
                    } else if self.target == EmitTarget::all() {
                        let ty = self.resolver.ensure_type_for_function_declaration(node);
                        let ty_str = self.resolver.print_type(ty);
                        if let Some(dts) = self.dts() {
                            dts.print().p(&ty_str);
                        }
                    }
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::Semi);
                    }
                    self.js.print().p_whitespace();
                    self.with_target(EmitTarget::JS, |this| this.emit_block_stmt(body));
                }
                (true, false) => self.with_target(EmitTarget::JS, |this| this.emit_fn_decl(node)),
                (false, true) => {
                    self.with_target(EmitTarget::DECLARATION, |this| this.emit_fn_decl(node))
                }
                (false, false) => {}
            }
        }
    }
    fn emit_class_decl(&mut self, node: &'cx ast::ClassDecl<'cx>) {
        if self.target == EmitTarget::JS {
            if node
                .modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
            {
                return;
            }
            self.emit_export_modifier_if_root(node.modifiers);
            self.emit_token(TokenKind::Class);
            self.js.print().p_whitespace();
            if let Some(name) = node.name {
                self.emit_ident(name);
                self.ns_names.insert((self.scope, name.name));
                self.js.print().p_whitespace();
            }
            if let Some(extends) = node.extends {
                self.emit_class_extends_clause_js(extends);
            }
            self.emit_class_body(node.elems, true);
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
            self.emit_default_modifier(node.modifiers);
            self.emit_declare_if_needed();
            self.dts.print().p_token(TokenKind::Class);
            self.dts.print().p_whitespace();
            if let Some(name) = node.name.map(|name| name.name) {
                self.dts.emit_atom(self.resolver.atoms(), name);
                self.dts.print().p_whitespace();
            }
            self.emit_type_parameters(node.ty_params);
            if let Some(extends) = node.extends {
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Extends);
                self.dts.print().p_whitespace();
                self.emit_expr(extends.expr_with_ty_args.expr);
                self.emit_type_arguments(extends.expr_with_ty_args.ty_args);
                self.dts.print().p_whitespace();
            }
            if let Some(implements) = node.implements {
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Implements);
                self.dts.print().p_whitespace();
                self.emit_list_indexed(
                    implements.list,
                    |this, item, _| {
                        this.emit_refer_ty(item);
                    },
                    |this, _, _| {
                        this.dts.print().p_token(TokenKind::Comma);
                        this.dts.print().p_whitespace();
                    },
                );

                self.dts.print().p_whitespace();
            }
            self.dts.print().p_token(TokenKind::LBrace);
            if !node.elems.list.is_empty() {
                self.dts.increment_indent();
                self.dts.print().p_newline();
                self.emit_list_indexed(
                    node.elems.list,
                    |this, elem, _| {
                        this.walk_class_elem(elem);
                    },
                    |this, _, _| {
                        this.dts.print().p_newline();
                    },
                );
                self.dts.decrement_indent();
                self.dts.print().p_newline();
            }
            self.dts.print().p_token(TokenKind::RBrace);
        } else {
            let js_on = !node
                .modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT));
            if !js_on {
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_class_decl(node));
                return;
            }
            self.emit_export_modifier_if_root(node.modifiers);
            self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
            self.emit_default_modifier(node.modifiers);
            self.emit_declare_if_needed();
            self.emit_token(TokenKind::Class);
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Class);
            }
            self.js.print().p_whitespace();
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            if let Some(name) = node.name {
                self.emit_ident(name);
                self.ns_names.insert((self.scope, name.name));
                self.js.print().p_whitespace();
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
            }
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_type_parameters(node.ty_params)
            });
            if let Some(extends) = node.extends {
                self.emit_token(TokenKind::Extends);
                self.js.print().p_whitespace();
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Extends);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                self.emit_expr(extends.expr_with_ty_args.expr);
                self.sync_comment_idx(extends.expr_with_ty_args.span.hi());
                if self.target == EmitTarget::all() {
                    self.with_target(EmitTarget::DECLARATION, |this| {
                        this.emit_type_arguments(extends.expr_with_ty_args.ty_args)
                    });
                }
                self.js.print().p_whitespace();
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
            }
            if let Some(implements) = node.implements {
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Implements);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                if self.target == EmitTarget::all() {
                    self.emit_list_indexed(
                        implements.list,
                        |this, item, _| {
                            this.emit_refer_ty(item);
                        },
                        |this, _, _| {
                            if let Some(dts) = this.dts() {
                                dts.print().p_token(TokenKind::Comma);
                            }
                            if let Some(dts) = this.dts() {
                                dts.print().p_whitespace();
                            }
                        },
                    );
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
            }
            self.emit_class_body(node.elems, true);
        }
    }
    fn emit_class_expr(&mut self, node: &'cx ast::ClassExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_class_expr(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Class);
            self.js.print().p_whitespace();
            if let Some(name) = node.name {
                self.emit_ident(name);
                self.ns_names.insert((self.scope, name.name));
                self.js.print().p_whitespace();
            }
            if let Some(extends) = node.extends {
                self.emit_class_extends_clause_js(extends);
            }
            self.emit_class_body(node.elems, false);
        } else {
            self.emit_token(TokenKind::Class);
            self.js.print().p_whitespace();
            if let Some(name) = node.name {
                self.emit_ident(name);
                self.ns_names.insert((self.scope, name.name));
                self.js.print().p_whitespace();
            }
            if let Some(ty_params) = node.ty_params {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty_param in ty_params {
                        this.emit_ty_param(ty_param);
                    }
                });
            }
            if let Some(extends) = node.extends {
                self.emit_class_extends_clause_js(extends);
            }
            if let Some(implements) = node.implements {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for item in implements.list {
                        this.emit_refer_ty(item);
                    }
                });
            }
            self.emit_class_body(node.elems, false);
        }
    }
    fn emit_throw_stmt(&mut self, node: &'cx ast::ThrowStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_throw_stmt(node)
        } else {
            self.emit_token(TokenKind::Throw);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
        }
    }
    fn emit_block_module_decl(&mut self, node: &'cx ast::BlockModuleDecl<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_instantiated_block_module_decl(node);
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_declare_if_needed();
            self.dts.print().p_token(TokenKind::Namespace);
            self.dts.print().p_whitespace();
            self.walk_module_name(node.name);
            if let Some(block) = node.block {
                self.emit_module_block(block)
            }
        } else {
            self.target = EmitTarget::JS;
            self.emit_block_module_decl(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_block_module_decl(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_nested_module_decl(&mut self, node: &'cx ast::NestedModuleDecl<'cx>) {
        if self.target == EmitTarget::JS {
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
                        this.js.content.p_newline();
                        this.emit_nested_module_decl(inner);
                        this.js.content.p_newline();

                        this.js.content.p(&param_name);
                        this.emit_token(TokenKind::Dot);
                        this.emit_ident(inner.name);
                        this.js.content.p_whitespace();
                        this.emit_token(TokenKind::Eq);
                        this.js.content.p_whitespace();
                        this.emit_ident(inner.name);
                        this.emit_token(TokenKind::Semi);
                        this.js.content.p_newline();
                    });
                }
                ast::NestedModuleBlock::Block(block) => {
                    self.emit_instantiated_block_module_decl(&ast::BlockModuleDecl {
                        id: node.id,
                        span: node.span,
                        modifiers: node.modifiers,
                        is_global_argument: false,
                        name: ast::ModuleName::Ident(node.name),
                        block: Some(block),
                    });
                }
            }
        } else if self.target == EmitTarget::DECLARATION {
            let n = node;
            self.emit_declare_if_needed();
            self.dts.print().p_token(TokenKind::Namespace);
            self.dts.print().p_whitespace();
            self.emit_ident(n.name);
            match n.block {
                ast::NestedModuleBlock::Nested(decl) => {
                    emit_nested_module_declaration_inner(self, decl);
                }
                ast::NestedModuleBlock::Block(block) => self.emit_module_block(block),
            }
        } else {
            self.target = EmitTarget::JS;
            self.emit_nested_module_decl(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_nested_module_decl(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) {
        if self.target == EmitTarget::JS {
            if node
                .modifiers
                .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
            {
                return;
            }
            self.emit_with_var_fn_wrapper(node.name, self.atoms().get(node.name.name), |this| {
                for member in node.members {
                    this.js.content.p_newline();
                    this.emit_ident(node.name);
                    this.emit_token(TokenKind::LBracket);
                    this.emit_ident(node.name);
                    this.emit_token(TokenKind::LBracket);
                    this.emit_enum_member_name(&member.name);
                    this.emit_token(TokenKind::RBracket);
                    this.js.content.p_whitespace();
                    this.emit_token(TokenKind::Eq);
                    this.js.content.p_whitespace();
                    if let Some(init) = member.init {
                        this.emit_expr(init);
                    } else {
                        this.js.content.p("0");
                    }
                    this.emit_token(TokenKind::RBracket);
                    this.js.content.p_whitespace();
                    this.emit_token(TokenKind::Eq);
                    this.js.content.p_whitespace();
                    this.emit_enum_member_name(&member.name);
                }
            });
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_declare_if_needed();
            self.dts.print().p_token(TokenKind::Enum);
            self.dts.print().p_whitespace();
            self.dts.emit_atom(self.resolver.atoms(), node.name.name);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::LBrace);
            self.dts.increment_indent();
            self.dts.print().p_newline();
            self.emit_list_indexed(
                node.members,
                |this, item, _| {
                    this.emit_enum_member(item);
                },
                |this, _, _| {
                    this.dts.print().p_token(TokenKind::Comma);
                    this.dts.print().p_newline();
                },
            );
            self.dts.decrement_indent();
            self.dts.print().p_newline();
            self.dts.print().p_token(TokenKind::RBrace);
        } else {
            self.target = EmitTarget::JS;
            self.emit_enum_decl(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_enum_decl(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_enum_member(&mut self, node: &'cx bolt_ts_ast::EnumMember<'cx>) {
        if self.target == EmitTarget::JS {
            // Enum members are emitted inline by emit_enum_decl.
        } else if self.target == EmitTarget::DECLARATION {
            self.walk_enum_member_name_kind(&node.name);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::Eq);
            self.dts.print().p_whitespace();
            let enum_member_value = self.resolver.get_enum_member_value(node);
            match enum_member_value {
                bolt_ts_checker::check::EnumMemberValue::Number(num) => {
                    self.dts.print().p(&num.to_string());
                }
                bolt_ts_checker::check::EnumMemberValue::Str(str) => {
                    self.dts.emit_atom(self.resolver.atoms(), str);
                }
                bolt_ts_checker::check::EnumMemberValue::Err => {
                    self.dts.print().p("/* computed value */");
                }
            };
        } else {
            self.target = EmitTarget::JS;
            self.emit_enum_member(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_enum_member(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Import);
            self.js.print().p_whitespace();
            if let Some(clause) = node.clause {
                self.emit_import_clause(clause);
            }
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::From);
            self.js.print().p_whitespace();
            self.emit_string_lit(node.module);
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::Import);
            self.dts.print().p_whitespace();
            if let Some(clause) = node.clause {
                self.emit_import_clause(clause);
                self.dts.print().p_whitespace();
            }
            self.dts.print().p_token(TokenKind::From);
            self.dts.print().p_whitespace();
            self.emit_string_literal(node.module.val);
        } else {
            self.emit_token(TokenKind::Import);
            self.js.print().p_whitespace();
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Import);
            }
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            if let Some(clause) = node.clause {
                self.emit_import_clause(clause);
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
            }
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::From);
            self.js.print().p_whitespace();
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::From);
            }
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            self.emit_string_lit(node.module);
        }
    }
    fn emit_import_clause(&mut self, node: &'cx ast::ImportClause<'cx>) {
        if self.target == EmitTarget::JS {
            if let Some(name) = node.name {
                self.emit_ident(name);
                self.js.print().p_whitespace();
            } else if let Some(kind) = node.kind {
                match kind {
                    ast::ImportClauseKind::Specs(specs) => {
                        self.emit_list(
                            specs,
                            |this, spec| this.emit_import_spec(spec),
                            |this, _| {
                                this.emit_token(TokenKind::Comma);
                                this.js.content.p_whitespace();
                            },
                        );
                    }
                    ast::ImportClauseKind::Ns(ns) => self.emit_ns_import(ns),
                }
            }
        } else if self.target == EmitTarget::DECLARATION {
            if let Some(name) = node.name {
                self.emit_ident(name);
            }
            if let Some(kind) = node.kind {
                match kind {
                    ast::ImportClauseKind::Ns(n) => self.emit_ns_import(n),
                    ast::ImportClauseKind::Specs(list) => {
                        self.dts.print().p_token(TokenKind::LBrace);
                        self.dts.print().p_whitespace();
                        self.emit_list_indexed(
                            list,
                            |this, item, _| match item.kind {
                                ast::ImportSpecKind::Named(n) => this.emit_import_named_spec(n),
                                ast::ImportSpecKind::Shorthand(n) => {
                                    this.emit_import_shorthand_spec(n)
                                }
                            },
                            |this, _, _| {
                                this.dts.print().p_token(TokenKind::Comma);
                                this.dts.print().p_whitespace();
                            },
                        );
                        self.dts.print().p_whitespace();
                        self.dts.print().p_token(TokenKind::RBrace);
                    }
                }
            }
        } else {
            if let Some(name) = node.name {
                self.emit_ident(name);
                self.js.print().p_whitespace();
                if let Some(kind) = node.kind {
                    match kind {
                        ast::ImportClauseKind::Ns(n) => {
                            self.with_target(EmitTarget::DECLARATION, |this| {
                                this.emit_ns_import(n)
                            });
                        }
                        ast::ImportClauseKind::Specs(list) => {
                            if let Some(dts) = self.dts() {
                                dts.print().p_token(TokenKind::LBrace);
                            }
                            if let Some(dts) = self.dts() {
                                dts.print().p_whitespace();
                            }
                            self.emit_list_indexed(
                                list,
                                |this, item, _| {
                                    this.with_target(EmitTarget::DECLARATION, |this| {
                                        match item.kind {
                                            ast::ImportSpecKind::Named(n) => {
                                                this.emit_import_named_spec(n)
                                            }
                                            ast::ImportSpecKind::Shorthand(n) => {
                                                this.emit_import_shorthand_spec(n)
                                            }
                                        }
                                    });
                                },
                                |this, _, _| {
                                    if let Some(dts) = this.dts() {
                                        dts.print().p_token(TokenKind::Comma);
                                    }
                                    if let Some(dts) = this.dts() {
                                        dts.print().p_whitespace();
                                    }
                                },
                            );
                            if let Some(dts) = self.dts() {
                                dts.print().p_whitespace();
                            }
                            if let Some(dts) = self.dts() {
                                dts.print().p_token(TokenKind::RBrace);
                            }
                        }
                    }
                }
            } else if let Some(kind) = node.kind {
                match kind {
                    ast::ImportClauseKind::Specs(specs) => {
                        if let Some(dts) = self.dts() {
                            dts.print().p_token(TokenKind::LBrace);
                        }
                        if let Some(dts) = self.dts() {
                            dts.print().p_whitespace();
                        }
                        for (idx, spec) in specs.iter().enumerate() {
                            if idx != 0 {
                                self.emit_token(TokenKind::Comma);
                                self.js.content.p_whitespace();
                                if let Some(dts) = self.dts() {
                                    dts.print().p_token(TokenKind::Comma);
                                }
                                if let Some(dts) = self.dts() {
                                    dts.print().p_whitespace();
                                }
                            }
                            self.emit_import_spec(spec);
                        }
                        if let Some(dts) = self.dts() {
                            dts.print().p_whitespace();
                        }
                        if let Some(dts) = self.dts() {
                            dts.print().p_token(TokenKind::RBrace);
                        }
                    }
                    ast::ImportClauseKind::Ns(ns) => self.emit_ns_import(ns),
                }
            }
        }
    }
    fn emit_ns_import(&mut self, node: &'cx ast::NsImport<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_ns_import(node)
        } else {
            self.emit_token(TokenKind::Asterisk);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::As);
            self.js.print().p_whitespace();
            self.emit_ident(node.name);
        }
    }
    fn emit_module_export_name(&mut self, node: &'cx ast::ModuleExportName<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_module_export_name(node)
        } else {
            match node.kind {
                ast::ModuleExportNameKind::Ident(ident) => self.emit_ident(ident),
                ast::ModuleExportNameKind::StringLit(lit) => self.emit_string_lit(lit),
            }
        }
    }
    fn emit_import_shorthand_spec(&mut self, node: &'cx ast::ImportShorthandSpec<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_ident(node.name);
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.emit_atom(self.resolver.atoms(), node.name.name);
        } else {
            self.emit_ident(node.name);
        }
    }
    fn emit_import_named_spec(&mut self, node: &'cx ast::ImportNamedSpec<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_module_export_name(node.prop_name);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::As);
            self.js.print().p_whitespace();
            self.emit_ident(node.name);
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.emit_atom(self.resolver.atoms(), node.name.name);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::As);
            self.dts.print().p_whitespace();
            match node.prop_name.kind {
                ast::ModuleExportNameKind::Ident(n) => {
                    self.emit_ident(n);
                }
                ast::ModuleExportNameKind::StringLit(n) => {
                    self.emit_string_lit(n);
                }
            }
        } else {
            self.target = EmitTarget::JS;
            self.emit_import_named_spec(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_import_named_spec(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_stmt(&mut self, node: &'cx ast::Stmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_stmt(node);
        } else {
            use ast::StmtKind::*;
            match node.kind {
                Var(n) => self.emit_var_stmt(n),
                Expr(n) => {
                    self.emit_expr_stmt(n);
                    if next_adjacent_token_on_this_line_is_semi(
                        self.origin.as_bytes(),
                        n.span.hi() as usize - 1,
                    ) {
                        self.emit_token(TokenKind::Semi);
                    } else {
                        self.js.print().p_token(TokenKind::Semi);
                    }
                }
                Fn(n) => self.emit_fn_decl(n),
                If(n) => self.emit_if_stmt(n),
                Block(n) => self.emit_block_stmt(n),
                Ret(n) => {
                    self.emit_ret_stmt(n);
                    self.emit_token(TokenKind::Semi);
                }
                Class(n) => self.emit_class_decl(n),
                Throw(n) => {
                    self.emit_throw_stmt(n);
                    if next_adjacent_token_on_this_line_is_semi(
                        self.origin.as_bytes(),
                        n.span.hi() as usize - 1,
                    ) {
                        self.emit_token(TokenKind::Semi);
                    } else {
                        self.js.print().p_token(TokenKind::Semi);
                    }
                }
                NestedModule(n) => self.emit_nested_module_decl(n),
                BlockModule(n) => self.emit_block_module_decl(n),
                Enum(n) => self.emit_enum_decl(n),
                Import(n) => self.emit_import_decl(n),
                ImportEquals(n) => self.emit_import_equals_decl(n),
                Export(n) => self.emit_export_decl(n),
                ExportAssign(n) => {
                    self.emit_export_assign(n);
                    self.emit_token(TokenKind::Semi);
                }
                For(n) => self.emit_for_stmt(n),
                ForOf(n) => self.emit_for_of_stmt(n),
                ForIn(n) => self.emit_for_in_stmt(n),
                Break(n) => {
                    self.emit_break_stmt(n);
                    self.emit_token(TokenKind::Semi);
                }
                Continue(n) => {
                    self.emit_continue_stmt(n);
                    self.emit_token(TokenKind::Semi);
                }
                Try(n) => self.emit_try_stmt(n),
                While(n) => self.emit_while_stmt(n),
                Do(n) => self.emit_do_while_stmt(n),
                Labeled(n) => self.emit_labeled_stmt(n),
                Empty(_) => self.emit_token(TokenKind::Semi),
                Switch(n) => self.emit_switch_stmt(n),
                Interface(n) => {
                    if self.target.contains(EmitTarget::DECLARATION) {
                        self.with_target(EmitTarget::DECLARATION, |this| {
                            this.emit_interface_decl(n)
                        });
                    }
                }
                TypeAlias(n) => {
                    if self.target.contains(EmitTarget::DECLARATION) {
                        self.with_target(EmitTarget::DECLARATION, |this| {
                            this.emit_type_alias_decl(n)
                        });
                    }
                }
                Debugger(_) => {}
            }
        }
    }
    fn emit_switch_stmt(&mut self, node: &'cx ast::SwitchStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_switch_stmt(node)
        } else {
            self.emit_token(TokenKind::Switch);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LParen);
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::RParen);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LBrace);
            self.emit_case_block(node.case_block);
            self.emit_token(TokenKind::RBrace);
        }
    }
    fn emit_case_block(&mut self, node: &'cx ast::CaseBlock<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_case_block(node)
        } else if self.target == EmitTarget::JS {
            if !node.clauses.is_empty() {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_list(
                node.clauses,
                |this, item| match item {
                    ast::CaseOrDefaultClause::Case(n) => this.emit_case_clause(n),
                    ast::CaseOrDefaultClause::Default(n) => this.emit_default_clause(n),
                },
                |this, _| {
                    this.js.content.p_newline();
                },
            );
            if !node.clauses.is_empty() {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
        } else {
            if !node.clauses.is_empty() {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_list(
                node.clauses,
                |this, item| match item {
                    ast::CaseOrDefaultClause::Case(n) => this.emit_case_clause(n),
                    ast::CaseOrDefaultClause::Default(n) => this.emit_default_clause(n),
                },
                |this, _| {
                    this.js.content.p_newline();
                },
            );
            if !node.clauses.is_empty() {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
        }
    }
    fn emit_case_clause(&mut self, node: &'cx ast::CaseClause<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_case_clause(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Case);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::Colon);
            let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
            if has_stmts {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_stmts_skip_omitted(node.stmts);
            if has_stmts {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
        } else {
            self.emit_token(TokenKind::Case);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::Colon);
            let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
            if has_stmts {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_stmts_skip_omitted(node.stmts);
            if has_stmts {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
        }
    }
    fn emit_default_clause(&mut self, node: &'cx ast::DefaultClause<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_default_clause(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Default);
            self.emit_token(TokenKind::Colon);
            let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
            if has_stmts {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_stmts_skip_omitted(node.stmts);
            if has_stmts {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
        } else {
            self.emit_token(TokenKind::Default);
            self.emit_token(TokenKind::Colon);
            let has_stmts = node.stmts.iter().any(|s| !self.stmt_is_omitted(s));
            if has_stmts {
                self.js.print().indent += self.js.options.indent;
                self.js.print().p_newline();
            }
            self.emit_stmts_skip_omitted(node.stmts);
            if has_stmts {
                self.js.print().indent -= self.js.options.indent;
                self.js.print().p_newline();
            }
        }
    }
    fn emit_export_assign(&mut self, node: &'cx ast::ExportAssign<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Export);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Default);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
        } else if self.target == EmitTarget::DECLARATION {
            if node.is_export_equals {
                self.dts.print().p_token(TokenKind::Export);
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Eq);
            } else {
                self.dts.print().p_token(TokenKind::Export);
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Default);
            }
            self.dts.print().p_whitespace();
            self.emit_expr(node.expr);
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.emit_token(TokenKind::Export);
            self.js.print().p_whitespace();
            if node.is_export_equals {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Export);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Eq);
                }
            } else {
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Export);
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_whitespace();
                }
                if let Some(dts) = self.dts() {
                    dts.print().p_token(TokenKind::Default);
                }
            }
            self.emit_token(TokenKind::Default);
            self.js.print().p_whitespace();
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            self.emit_expr(node.expr);
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Semi);
            }
        }
    }
    fn emit_labeled_stmt(&mut self, node: &'cx ast::LabeledStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_labeled_stmt(node)
        } else {
            self.emit_ident(node.label);
            self.emit_token(TokenKind::Colon);
            self.js.print().p_whitespace();
            self.emit_stmt(node.stmt);
        }
    }
    fn emit_do_while_stmt(&mut self, node: &'cx ast::DoWhileStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_do_while_stmt(node)
        } else {
            self.emit_token(TokenKind::Do);
            self.js.print().p_whitespace();
            self.emit_stmt(node.stmt);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::While);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LParen);
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::RParen);
        }
    }
    fn emit_while_stmt(&mut self, node: &'cx ast::WhileStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_while_stmt(node)
        } else {
            self.emit_token(TokenKind::While);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LParen);
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::RParen);
            self.js.print().p_whitespace();
            self.emit_stmt(node.stmt);
        }
    }
    fn emit_catch_clause(&mut self, node: &'cx ast::CatchClause<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_catch_clause(node)
        } else {
            self.emit_token(TokenKind::Catch);
            self.js.print().p_whitespace();
            if let Some(var) = node.var {
                self.emit_token(TokenKind::LParen);
                self.emit_var_decl(var);
                self.emit_token(TokenKind::RParen);
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(node.block);
        }
    }
    fn emit_try_stmt(&mut self, node: &'cx ast::TryStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_try_stmt(node)
        } else {
            self.emit_token(TokenKind::Try);
            self.js.print().p_whitespace();
            self.emit_block_stmt(node.try_block);
            if let Some(catch) = node.catch_clause {
                self.js.print().p_whitespace();
                self.emit_catch_clause(catch);
            }
            if let Some(finally) = node.finally_block {
                self.emit_token(TokenKind::Finally);
                self.js.print().p_whitespace();
                self.emit_block_stmt(finally);
            }
        }
    }
    fn emit_continue_stmt(&mut self, node: &'cx ast::ContinueStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_continue_stmt(node)
        } else {
            self.emit_token(TokenKind::Continue);
            if let Some(label) = node.label {
                self.js.print().p_whitespace();
                self.emit_ident(label);
            }
        }
    }
    fn emit_break_stmt(&mut self, node: &'cx ast::BreakStmt<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_break_stmt(node)
        } else {
            self.emit_token(TokenKind::Break);
            if let Some(label) = node.label {
                self.js.print().p_whitespace();
                self.emit_ident(label);
            }
        }
    }
    fn emit_for_in_stmt(&mut self, node: &'cx ast::ForInStmt<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::For);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LParen);
            self.js.print().p_whitespace();
            self.emit_for_init(node.init);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::In);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::RParen);
            self.js.print().p_whitespace();
            self.emit_stmt(node.body);
        } else if self.target == EmitTarget::DECLARATION {
        } else {
            self.with_target(EmitTarget::JS, |this| this.emit_for_in_stmt(node));
        }
    }
    fn emit_for_stmt(&mut self, node: &'cx ast::ForStmt<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::For);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::LParen);
            self.js.print().p_whitespace();
            if let Some(init) = node.init {
                self.emit_for_init(init);
            }
            self.emit_token(TokenKind::Semi);
            self.js.print().p_whitespace();
            if let Some(cond) = node.cond {
                self.emit_expr(cond);
            }
            self.emit_token(TokenKind::Semi);
            self.js.print().p_whitespace();
            if let Some(incr) = node.incr {
                self.emit_expr(incr);
            }
            self.emit_token(TokenKind::RParen);
            self.js.print().p_whitespace();
            self.emit_stmt(node.body);
        } else if self.target == EmitTarget::DECLARATION {
        } else {
            self.with_target(EmitTarget::JS, |this| this.emit_for_stmt(node));
        }
    }
    fn emit_for_of_stmt(&mut self, node: &'cx ast::ForOfStmt<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::For);
            self.js.print().p_whitespace();
            if node.r#await.is_some() {
                self.emit_token(TokenKind::Await);
                self.js.print().p_whitespace();
            }
            self.emit_token(TokenKind::LParen);
            self.js.print().p_whitespace();
            self.emit_for_init(node.init);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Of);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::RParen);
            self.js.print().p_whitespace();
            self.emit_stmt(node.body);
        } else if self.target == EmitTarget::DECLARATION {
        } else {
            self.with_target(EmitTarget::JS, |this| this.emit_for_of_stmt(node));
        }
    }
    fn emit_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_export_decl(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Export);
            self.js.print().p_whitespace();
            match node.clause.kind {
                ast::ExportClauseKind::Specs(specs) => {
                    self.emit_token(TokenKind::LBrace);
                    self.js.print().p_whitespace();
                    self.emit_list(
                        specs.list,
                        |this, spec| this.emit_export_spec(spec),
                        |this, _| {
                            this.emit_token(TokenKind::Comma);
                            this.js.content.p_whitespace();
                        },
                    );
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::RBrace);
                    if let Some(module) = specs.module {
                        self.js.print().p_whitespace();
                        self.emit_token(TokenKind::From);
                        self.js.print().p_whitespace();
                        self.emit_string_lit(module);
                    }
                }
                ast::ExportClauseKind::Ns(n) => self.emit_ns_export(n),
                ast::ExportClauseKind::Glob(n) => {
                    self.emit_token(TokenKind::Asterisk);
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::From);
                    self.js.print().p_whitespace();
                    self.emit_string_lit(n.module);
                }
            }
        } else {
            self.emit_token(TokenKind::Export);
            self.js.print().p_whitespace();
            match node.clause.kind {
                ast::ExportClauseKind::Specs(specs) => {
                    self.emit_token(TokenKind::LBrace);
                    self.js.print().p_whitespace();
                    self.emit_list(
                        specs.list,
                        |this, spec| this.emit_export_spec(spec),
                        |this, _| {
                            this.emit_token(TokenKind::Comma);
                            this.js.content.p_whitespace();
                        },
                    );
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::RBrace);
                    if let Some(module) = specs.module {
                        self.js.print().p_whitespace();
                        self.emit_token(TokenKind::From);
                        self.js.print().p_whitespace();
                        self.emit_string_lit(module);
                    }
                }
                ast::ExportClauseKind::Ns(n) => self.emit_ns_export(n),
                ast::ExportClauseKind::Glob(n) => {
                    self.emit_token(TokenKind::Asterisk);
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::From);
                    self.js.print().p_whitespace();
                    self.emit_string_lit(n.module);
                }
            }
        }
    }
    fn emit_export_named_spec(&mut self, node: &'cx ast::ExportNamedSpec<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_export_named_spec(node)
        } else if self.target == EmitTarget::JS {
            self.emit_module_export_name(node.prop_name);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::As);
            self.js.print().p_whitespace();
            self.emit_module_export_name(node.name);
        } else {
            self.target = EmitTarget::JS;
            self.emit_export_named_spec(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_export_named_spec(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_export_shorthand_spec(&mut self, node: &'cx ast::ExportShorthandSpec<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_export_shorthand_spec(node)
        } else {
            self.emit_ident(node.name);
        }
    }
    fn emit_ns_export(&mut self, node: &'cx ast::NsExport<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_ns_export(node)
        } else {
            self.emit_token(TokenKind::Asterisk);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::As);
            self.js.print().p_whitespace();
            self.emit_module_export_name(node.name);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::From);
            self.js.print().p_whitespace();
            self.emit_string_lit(node.module);
        }
    }
    fn emit_assign_expr(&mut self, node: &'cx ast::AssignExpr<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_expr(node.left);
            self.js.print().p_whitespace();
            let token = match node.op {
                ast::AssignOp::LogicalAndEq | ast::AssignOp::LogicalOrEq => TokenKind::Unknown,
                _ => node.op.into(),
            };
            self.emit_token_value(token, node.op.as_str());
            self.js.print().p_whitespace();
            self.emit_expr(node.right);
        } else if self.target == EmitTarget::DECLARATION {
        } else {
            self.with_target(EmitTarget::JS, |this| this.emit_assign_expr(node));
        }
    }
    fn emit_bin_expr(&mut self, node: &'cx ast::BinExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_bin_expr(node)
        } else {
            self.emit_expr(node.left);
            self.js.print().p_whitespace();
            self.emit_token_value(node.op.into(), node.op.as_str());
            self.js.print().p_whitespace();
            self.emit_expr(node.right);
        }
    }
    fn emit_paren_expr(&mut self, node: &'cx ast::ParenExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_paren_expr(node)
        } else {
            self.emit_token(TokenKind::LParen);
            self.emit_expr(node.expr);
            self.emit_token(TokenKind::RParen);
        }
    }
    fn emit_this_expr(&mut self, _node: &'cx ast::ThisExpr) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_this_expr(_node)
        } else {
            self.emit_token(TokenKind::This);
        }
    }
    fn emit_big_int_lit(&mut self, node: &'cx ast::BigIntLit) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_big_int_lit(node)
        } else {
            if node.val.0 {
                self.emit_token(TokenKind::Minus);
            }
            let content = self.atoms().get(node.val.1);
            self.emit_token_value(TokenKind::BigInt, &format!("{content}n"));
        }
    }
    fn emit_reg_exp_lit(&mut self, node: &'cx ast::RegExpLit) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_reg_exp_lit(node)
        } else {
            let content = self.atoms().get(node.val);
            self.emit_token_value(TokenKind::Regexp, content);
        }
    }
    fn emit_array_lit(&mut self, node: &'cx ast::ArrayLit<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_array_lit(node)
        } else {
            self.emit_token(TokenKind::LBracket);
            for (idx, expr) in node.elems.iter().enumerate() {
                self.emit_expr(expr);
                if idx != node.elems.len() - 1 {
                    self.emit_token(TokenKind::Comma);
                    self.js.print().p_whitespace();
                }
            }
            self.emit_token(TokenKind::RBracket);
        }
    }
    fn emit_object_lit(&mut self, node: &'cx ast::ObjectLit<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_object_lit(node)
        } else if self.target == EmitTarget::JS {
            if node.members.is_empty() {
                self.emit_token(TokenKind::LBrace);
                self.emit_token(TokenKind::RBrace);
                return;
            }
            self.emit_token(TokenKind::LBrace);
            self.js.print().p_newline();
            self.js.print().indent += self.js.options.indent;
            let indent = self.js.print().indent;
            self.js.print().p_pieces_of_whitespace(indent);
            self.emit_list(
                node.members,
                |this, member| {
                    this.emit_object_member(member);
                },
                |this, _| {
                    this.emit_token(TokenKind::Comma);
                    this.js.content.p_newline();
                },
            );
            let indent = self.js.print().indent;
            self.js.print().p_pieces_of_whitespace(indent);
            self.js.print().indent -= self.js.options.indent;
            self.js.print().p_newline();
            self.emit_token(TokenKind::RBrace);
        } else {
            if node.members.is_empty() {
                self.emit_token(TokenKind::LBrace);
                self.emit_token(TokenKind::RBrace);
                return;
            }
            self.emit_token(TokenKind::LBrace);
            self.js.print().p_newline();
            self.js.print().indent += self.js.options.indent;
            let indent = self.js.print().indent;
            self.js.print().p_pieces_of_whitespace(indent);
            self.emit_list(
                node.members,
                |this, member| {
                    this.emit_object_member(member);
                },
                |this, _| {
                    this.emit_token(TokenKind::Comma);
                    this.js.content.p_newline();
                },
            );
            let indent = self.js.print().indent;
            self.js.print().p_pieces_of_whitespace(indent);
            self.js.print().indent -= self.js.options.indent;
            self.js.print().p_newline();
            self.emit_token(TokenKind::RBrace);
        }
    }
    fn emit_object_method_member(&mut self, node: &'cx ast::ObjectMethodMember<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_object_method_member(node)
        } else if self.target == EmitTarget::JS {
            if node.asterisk.is_some() {
                self.emit_token(TokenKind::Asterisk);
            }
            self.emit_prop_name(node.name);
            self.emit_params_without_this(node.params);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(node.body);
        } else {
            if node.asterisk.is_some() {
                self.emit_token(TokenKind::Asterisk);
            }
            self.emit_prop_name(node.name);
            if let Some(ty_params) = node.ty_params {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty_param in ty_params {
                        this.emit_ty_param(ty_param);
                    }
                });
            }
            self.emit_fn_params(node.params, false);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(node.body);
        }
    }
    fn emit_object_prop_assignment(&mut self, node: &'cx ast::ObjectPropAssignment<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_object_prop_assignment(node)
        } else {
            self.emit_prop_name(node.name);
            self.emit_token(TokenKind::Colon);
            self.js.print().p_whitespace();
            self.emit_expr(node.init);
        }
    }
    fn emit_object_shorthand_member(&mut self, node: &'cx ast::ObjectShorthandMember<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_object_shorthand_member(node)
        } else if self.target == EmitTarget::JS {
            self.emit_ident(node.name);
        } else {
            self.emit_ident(node.name);
            if let Some(init) = node.object_assignment_initializer {
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_expr(init));
            }
        }
    }
    fn emit_prop_access_expr(&mut self, node: &'cx ast::PropAccessExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_prop_access_expr(node)
        } else {
            if let ast::ExprKind::NumLit(n) = node.expr.kind {
                self.emit_num_lit(n);
                if n.val.fract() == 0. {
                    self.emit_token(TokenKind::Dot);
                }
            } else {
                self.emit_expr(node.expr);
            }
            self.emit_token(TokenKind::Dot);
            self.emit_ident(node.name);
        }
    }
    fn emit_template_expr(&mut self, node: &'cx ast::TemplateExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_template_expr(node)
        } else {
            // emit_template_head
            self.emit_token(TokenKind::Backtick);
            let content = self.atoms().get(node.head.text);
            let content = escape_snippet_text(content);
            self.js.print().p(&content);
            for span in node.spans {
                self.js.print().p_dollar_and_brace();
                self.emit_expr(span.expr);
                self.emit_token(TokenKind::RBrace);
                let content = self.atoms().get(span.text);
                self.js.print().p(content);
            }
            self.js.print().p_token(TokenKind::Backtick);
        }
    }
    fn emit_no_substitution_template_lit(&mut self, node: &'cx ast::NoSubstitutionTemplateLit) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_no_substitution_template_lit(node)
        } else {
            let content = get_source_text_from_source(&self.origin, node.span).to_string();
            self.emit_token_value(TokenKind::NoSubstitutionTemplate, &content);
        }
    }

    fn emit_expr(&mut self, node: &'cx ast::Expr<'cx>) {
        use ast::ExprKind::*;
        match node.kind {
            Assign(n) => self.emit_assign_expr(n),
            Bin(n) => self.emit_bin_expr(n),
            Omit(_) => {}
            Paren(n) => self.emit_paren_expr(n),
            This(n) => self.emit_this_expr(n),
            Ident(n) => self.emit_ident(n),
            BoolLit(n) => {
                if n.val {
                    self.emit_token(TokenKind::True);
                } else {
                    self.emit_token(TokenKind::False);
                }
            }
            NullLit(_) => self.emit_token(TokenKind::Null),
            NumLit(n) => self.emit_num_lit(n),
            BigIntLit(n) => self.emit_big_int_lit(n),
            RegExpLit(n) => self.emit_reg_exp_lit(n),
            StringLit(n) => self.emit_string_lit(n),
            NoSubstitutionTemplateLit(n) => self.emit_no_substitution_template_lit(n),
            ArrayLit(n) => self.emit_array_lit(n),
            ObjectLit(n) => self.emit_object_lit(n),
            Void(n) => {
                self.emit_token(TokenKind::Void);
                self.js.print().p_whitespace();
                self.emit_expr(n.expr);
            }
            Typeof(n) => {
                self.emit_token(TokenKind::Typeof);
                self.js.print().p_whitespace();
                self.emit_expr(n.expr);
            }
            Super(_) => self.emit_token(TokenKind::Super),
            EleAccess(n) => {
                self.emit_expr(n.expr);
                self.emit_token(TokenKind::LBracket);
                self.emit_expr(n.arg);
                self.emit_token(TokenKind::RBracket);
            }
            PropAccess(n) => self.emit_prop_access_expr(n),
            PostfixUnary(n) => {
                self.emit_expr(n.expr);
                let token = match n.op {
                    ast::PostfixUnaryOp::PlusPlus => TokenKind::PlusPlus,
                    ast::PostfixUnaryOp::MinusMinus => TokenKind::MinusMinus,
                };
                self.emit_token_value(token, n.op.as_str());
            }
            PrefixUnary(n) => {
                let token = match n.op {
                    ast::PrefixUnaryOp::Plus => TokenKind::Plus,
                    ast::PrefixUnaryOp::Minus => TokenKind::Minus,
                    ast::PrefixUnaryOp::PlusPlus => TokenKind::PlusPlus,
                    ast::PrefixUnaryOp::MinusMinus => TokenKind::MinusMinus,
                    ast::PrefixUnaryOp::Tilde => TokenKind::Tilde,
                    ast::PrefixUnaryOp::Excl => TokenKind::Excl,
                };
                self.emit_token_value(token, n.op.as_str());
                if matches!(n.expr.kind, PrefixUnary(_)) {
                    self.js.print().p_whitespace();
                }
                self.emit_expr(n.expr);
            }
            TaggedTemplate(n) => {
                self.emit_expr(n.tag);
                if let Some(ty_args) = n.ty_args {
                    self.with_target(EmitTarget::DECLARATION, |this| {
                        for ty in ty_args.list {
                            this.emit_ty(ty);
                        }
                    });
                }
                match n.tpl {
                    ast::TemplateExpressionKind::NoSubstitutionTemplateLit(n) => {
                        self.emit_no_substitution_template_lit(n);
                    }
                    ast::TemplateExpressionKind::TemplateExpr(n) => {
                        self.emit_template_expr(n);
                    }
                }
            }
            Template(n) => self.emit_template_expr(n),
            SpreadElement(n) => {
                self.emit_token(TokenKind::DotDotDot);
                self.emit_expr(n.expr);
            }
            ArrowFn(n) => self.emit_arrow_fn_expr(n),
            New(n) => self.emit_new_expr(n),
            Class(n) => self.emit_class_expr(n),
            Fn(n) => self.emit_fn_expr(n),
            Call(n) => self.emit_call_expr(n),
            Cond(n) => self.emit_cond_expr(n),
            JsxElem(n) => self.emit_jsx_elem(n),
            JsxSelfClosingElem(n) => self.emit_jsx_self_closing_elem(n),
            JsxFrag(n) => self.emit_jsx_frag(n),
            Delete(n) => {
                self.emit_token(TokenKind::Delete);
                self.js.print().p_whitespace();
                self.emit_expr(n.expr);
            }
            Await(n) => {
                self.emit_token(TokenKind::Await);
                self.js.print().p_whitespace();
                self.emit_expr(n.expr);
            }
            Yield(n) => {
                self.emit_token(TokenKind::Yield);
                self.js.print().p_whitespace();
                if n.asterisk.is_some() {
                    self.emit_token(TokenKind::Asterisk);
                    self.js.print().p_whitespace();
                }
                if let Some(expr) = n.expr {
                    self.emit_expr(expr);
                }
            }
            NewMetaProperty(n) => {
                self.emit_token(TokenKind::New);
                self.emit_token(TokenKind::Dot);
                self.emit_ident(n.name);
            }
            As(n) => {
                self.emit_expr(n.expr);
                self.sync_comment_idx(n.span.hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(n.ty));
            }
            Satisfies(n) => {
                self.emit_expr(n.expr);
                self.sync_comment_idx(n.span.hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(n.ty));
            }
            NonNull(n) => {
                self.emit_expr(n.expr);
                self.sync_comment_idx(n.span.hi());
            }
            TyAssertion(n) => {
                self.sync_comment_idx(n.expr.span().lo());
                if matches!(n.expr.kind, ast::ExprKind::ObjectLit(_)) {
                    self.js.print().p_token(TokenKind::LParen);
                    self.emit_expr(n.expr);
                    self.js.print().p_token(TokenKind::RParen);
                } else {
                    self.emit_expr(n.expr);
                }
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(n.ty));
            }
            ExprWithTyArgs(n) => self.emit_expr_with_ty_args(n),
            Import(n) => self.emit_import_expression(n),
        }
    }

    fn emit_jsx_ns_name(&mut self, node: &'cx ast::JsxNsName<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_jsx_ns_name(node)
        } else {
            self.emit_ident(node.ns);
            self.emit_token(TokenKind::Colon);
            self.emit_ident(node.name);
        }
    }
    fn emit_jsx_expr(&mut self, node: &'cx ast::JsxExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_jsx_expr(node)
        } else {
            self.emit_token(TokenKind::LBrace);
            if node.dotdotdot_token.is_some() {
                self.emit_token(TokenKind::DotDotDot);
            }
            if let Some(expr) = node.expr {
                self.emit_expr(expr);
            }
            self.emit_token(TokenKind::RBrace);
        }
    }
    fn emit_jsx_frag(&mut self, node: &'cx ast::JsxFrag<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_jsx_frag(node)
        } else {
            self.emit_token(TokenKind::Less);
            self.emit_token(TokenKind::Great);
            for child in node.children {
                self.emit_jsx_child(*child);
            }
            self.emit_token(TokenKind::LessSlash);
            self.emit_token(TokenKind::Great);
        }
    }
    fn emit_jsx_self_closing_elem(&mut self, node: &'cx ast::JsxSelfClosingElem<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_jsx_self_closing_elem(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Less);
            self.emit_jsx_tag_name(node.tag_name);
            self.js.print().p_whitespace();
            for attr in node.attrs {
                self.js.print().p_whitespace();
                self.emit_jsx_attr(attr);
            }
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Slash);
            self.emit_token(TokenKind::Great);
        } else {
            self.emit_token(TokenKind::Less);
            self.emit_jsx_tag_name(node.tag_name);
            if let Some(ty_args) = node.ty_args {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty in ty_args.list {
                        this.emit_ty(ty);
                    }
                });
            }
            self.js.print().p_whitespace();
            for attr in node.attrs {
                self.js.print().p_whitespace();
                self.emit_jsx_attr(attr);
            }
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Slash);
            self.emit_token(TokenKind::Great);
        }
    }
    fn emit_jsx_elem(&mut self, node: &'cx ast::JsxElem<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_jsx_elem(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Less);
            self.emit_jsx_tag_name(node.opening_elem.tag_name);
            self.js.print().p_whitespace();
            for attr in node.opening_elem.attrs {
                self.js.print().p_whitespace();
                self.emit_jsx_attr(attr);
            }
            self.emit_token(TokenKind::Great);

            for child in node.children {
                self.emit_jsx_child(*child);
            }

            self.emit_token(TokenKind::LessSlash);
            self.emit_jsx_tag_name(node.closing_elem.tag_name);
            self.emit_token(TokenKind::Great);
        } else {
            self.emit_token(TokenKind::Less);
            self.emit_jsx_tag_name(node.opening_elem.tag_name);
            if let Some(ty_args) = node.opening_elem.ty_args {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty in ty_args.list {
                        this.emit_ty(ty);
                    }
                });
            }
            self.js.print().p_whitespace();
            for attr in node.opening_elem.attrs {
                self.js.print().p_whitespace();
                self.emit_jsx_attr(attr);
            }
            self.emit_token(TokenKind::Great);

            for child in node.children {
                self.emit_jsx_child(*child);
            }

            self.emit_token(TokenKind::LessSlash);
            self.emit_jsx_tag_name(node.closing_elem.tag_name);
            self.emit_token(TokenKind::Great);
        }
    }
    fn emit_cond_expr(&mut self, node: &'cx ast::CondExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_cond_expr(node)
        } else {
            self.emit_expr(node.cond);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Question);
            self.js.print().p_whitespace();
            self.emit_expr(node.when_true);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Colon);
            self.js.print().p_whitespace();
            self.emit_expr(node.when_false);
        }
    }
    fn emit_fn_expr(&mut self, node: &'cx ast::FnExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_fn_expr(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::Function);
            if node.asterisk.is_some() {
                self.emit_token(TokenKind::Asterisk);
            }
            self.js.print().p_whitespace();
            if let Some(name) = node.name {
                self.emit_ident(name);
            }
            self.emit_params_without_this(node.params);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(node.body);
        } else {
            self.emit_token(TokenKind::Function);
            if node.asterisk.is_some() {
                self.emit_token(TokenKind::Asterisk);
            }
            self.js.print().p_whitespace();
            if let Some(name) = node.name {
                self.emit_ident(name);
            }
            if let Some(ty_params) = node.ty_params {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty_param in ty_params {
                        this.emit_ty_param(ty_param);
                    }
                });
            }
            self.emit_fn_params(node.params, false);
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
            }
            self.js.print().p_whitespace();
            self.emit_block_stmt(node.body);
        }
    }
    fn emit_call_expr(&mut self, node: &'cx ast::CallExpr<'cx>) {
        if self.target == EmitTarget::JS {
            self.emit_expr(node.expr);
            self.emit_args(node.args);
        } else if self.target == EmitTarget::DECLARATION {
        } else {
            self.with_target(EmitTarget::JS, |this| this.emit_call_expr(node));
        }
    }
    fn emit_new_expr(&mut self, node: &'cx ast::NewExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_new_expr(node)
        } else if self.target == EmitTarget::JS {
            self.emit_token(TokenKind::New);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
            match node.args {
                Some(args) => self.emit_args(args),
                None => {
                    self.js.print().p_token(TokenKind::LParen);
                    self.js.print().p_token(TokenKind::RParen);
                }
            }
        } else {
            self.emit_token(TokenKind::New);
            self.js.print().p_whitespace();
            self.emit_expr(node.expr);
            if let Some(ty_args) = node.ty_args {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty in ty_args.list {
                        this.emit_ty(ty);
                    }
                });
            }
            match node.args {
                Some(args) => self.emit_args(args),
                None => {
                    self.js.print().p_token(TokenKind::LParen);
                    self.js.print().p_token(TokenKind::RParen);
                }
            }
        }
    }
    fn emit_expr_with_ty_args(&mut self, node: &'cx ast::ExprWithTyArgs<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_expr_with_ty_args(node)
        } else if self.target == EmitTarget::JS {
            self.emit_expr(node.expr);
            self.sync_comment_idx(node.span.hi());
        } else {
            self.emit_expr(node.expr);
            self.sync_comment_idx(node.span.hi());
            if let Some(ty_args) = node.ty_args {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty in ty_args.list {
                        this.emit_ty(ty);
                    }
                });
            }
        }
    }
    fn emit_arrow_fn_expr(&mut self, node: &'cx ast::ArrowFnExpr<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_arrow_fn_expr(node)
        } else if self.target == EmitTarget::JS {
            if node.async_modifier.is_some() {
                self.emit_token(TokenKind::Async);
                self.js.print().p_whitespace();
            }
            let single_ident_param = match node.params {
                [param] => {
                    matches!(param.name.kind, ast::BindingKind::Ident(_))
                        && param.modifiers.is_none()
                        && param.dotdotdot.is_none()
                        && param.question.is_none()
                        && param.ty.is_none()
                        && param.init.is_none()
                        && !self.origin[node.span.lo() as usize..param.span.lo() as usize]
                            .contains('(')
                }
                _ => false,
            };
            if single_ident_param {
                self.js.print().p_token(TokenKind::LParen);
                self.emit_param_decl(node.params[0]);
                self.js.print().p_token(TokenKind::RParen);
            } else {
                self.emit_params_without_this(node.params);
            }
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
            }
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::EqGreat);
            self.js.print().p_whitespace();

            match node.body {
                ast::ArrowFnExprBody::Expr(expr) => {
                    self.js.print().p_token(TokenKind::LParen);
                    self.emit_expr(expr);
                    self.js.print().p_token(TokenKind::RParen);
                }
                ast::ArrowFnExprBody::Block(block) => {
                    let mut non_omitted = block.stmts.iter().filter(|s| !self.stmt_is_omitted(s));
                    let single = non_omitted.next().filter(|_| non_omitted.next().is_none());
                    if let Some(stmt) = single
                        && let ast::StmtKind::Ret(ret) = stmt.kind
                        && let Some(expr) = ret.expr
                    {
                        self.emit_token(TokenKind::LParen);
                        self.emit_expr(expr);
                        self.emit_token(TokenKind::RParen);
                    } else {
                        self.emit_block_stmt(block);
                    }
                }
            }
        } else {
            if node.async_modifier.is_some() {
                self.emit_token(TokenKind::Async);
                self.js.print().p_whitespace();
            }
            if let Some(ty_params) = node.ty_params {
                self.with_target(EmitTarget::DECLARATION, |this| {
                    for ty_param in ty_params {
                        this.emit_ty_param(ty_param);
                    }
                });
            }
            let single_ident_param = match node.params {
                [param] => {
                    matches!(param.name.kind, ast::BindingKind::Ident(_))
                        && param.modifiers.is_none()
                        && param.dotdotdot.is_none()
                        && param.question.is_none()
                        && param.ty.is_none()
                        && param.init.is_none()
                        && !self.origin[node.span.lo() as usize..param.span.lo() as usize]
                            .contains('(')
                }
                _ => false,
            };
            if single_ident_param {
                self.js.print().p_token(TokenKind::LParen);
                self.emit_param_decl(node.params[0]);
                self.js.print().p_token(TokenKind::RParen);
            } else {
                self.emit_fn_params(node.params, false);
            }
            if let Some(ty) = node.ty {
                self.sync_comment_idx(ty.span().hi());
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(ty));
            }
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::EqGreat);
            self.js.print().p_whitespace();

            match node.body {
                ast::ArrowFnExprBody::Expr(expr) => {
                    self.js.print().p_token(TokenKind::LParen);
                    self.emit_expr(expr);
                    self.js.print().p_token(TokenKind::RParen);
                }
                ast::ArrowFnExprBody::Block(block) => {
                    let mut non_omitted = block.stmts.iter().filter(|s| !self.stmt_is_omitted(s));
                    let single = non_omitted.next().filter(|_| non_omitted.next().is_none());
                    if let Some(stmt) = single
                        && let ast::StmtKind::Ret(ret) = stmt.kind
                        && let Some(expr) = ret.expr
                    {
                        for s in block.stmts {
                            if self.stmt_is_omitted(s) {
                                self.with_target(EmitTarget::DECLARATION, |this| this.emit_stmt(s));
                            }
                        }
                        self.emit_token(TokenKind::LParen);
                        self.emit_expr(expr);
                        self.emit_token(TokenKind::RParen);
                    } else {
                        self.emit_block_stmt(block);
                    }
                }
            }
        }
    }
    fn emit_import_equals_decl(&mut self, node: &'cx ast::ImportEqualsDecl<'cx>) {
        if self.target == EmitTarget::JS {
            if self.resolver.is_import_equals_namespace_module(node) {
                return;
            }
            self.emit_token(TokenKind::Var);
            self.js.print().p_whitespace();
            self.emit_ident(node.name);
            self.js.print().p_whitespace();
            self.emit_token(TokenKind::Eq);
            self.js.print().p_whitespace();
            match node.module_reference {
                ast::ModuleReferenceKind::EntityName(n) => self.emit_entity_name(n),
                ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                    self.emit_string_lit(n.module_spec())
                }
            }
        } else if self.target == EmitTarget::DECLARATION {
            if node.export_modifier.is_some() {
                self.dts.print().p_token(TokenKind::Export);
                self.dts.print().p_whitespace();
            }
            self.dts.print().p_token(TokenKind::Import);
            self.dts.print().p_whitespace();
            self.emit_ident(node.name);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::Eq);
            self.dts.print().p_whitespace();
            match node.module_reference {
                ast::ModuleReferenceKind::EntityName(n) => {
                    self.emit_entity_name(n);
                }
                ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                    self.emit_string_lit(n.module_spec());
                }
            }
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            let js_on = !self.resolver.is_import_equals_namespace_module(node);
            if js_on {
                self.emit_token(TokenKind::Var);
                self.js.print().p_whitespace();
            }
            if let Some(dts) = self.dts() {
                if node.export_modifier.is_some() {
                    dts.print().p_token(TokenKind::Export);
                    dts.print().p_whitespace();
                }
                dts.print().p_token(TokenKind::Import);
                dts.print().p_whitespace();
            }
            if js_on {
                self.emit_ident(node.name);
            } else {
                self.with_target(EmitTarget::DECLARATION, |this| this.emit_ident(node.name));
            }
            if js_on {
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
            }
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Eq);
            }
            if let Some(dts) = self.dts() {
                dts.print().p_whitespace();
            }
            match node.module_reference {
                ast::ModuleReferenceKind::EntityName(n) => {
                    if js_on {
                        self.emit_entity_name(n);
                    } else {
                        self.with_target(EmitTarget::DECLARATION, |this| this.emit_entity_name(n));
                    }
                }
                ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                    if js_on {
                        self.emit_string_lit(n.module_spec());
                    } else {
                        self.with_target(EmitTarget::DECLARATION, |this| {
                            this.emit_string_lit(n.module_spec())
                        });
                    }
                }
            }
            if let Some(dts) = self.dts() {
                dts.print().p_token(TokenKind::Semi);
            }
        }
    }
    fn emit_qualified_name(&mut self, node: &'cx ast::QualifiedName<'cx>) {
        if self.target == EmitTarget::DECLARATION {
            self.walk_qualified_name(node)
        } else if self.target == EmitTarget::JS {
            self.emit_entity_name(node.left);
            self.emit_token(TokenKind::Dot);
            self.emit_ident(node.right);
        } else {
            self.target = EmitTarget::JS;
            self.emit_qualified_name(node);
            self.target = EmitTarget::all();
            if self.target == EmitTarget::all() {
                self.target = EmitTarget::DECLARATION;
                self.emit_qualified_name(node);
                self.target = EmitTarget::all();
            }
        }
    }
    fn emit_entity_name(&mut self, node: &'cx ast::EntityName<'cx>) {
        if self.target == EmitTarget::JS {
            use ast::EntityNameKind::*;
            match &node.kind {
                Ident(n) => self.emit_ident(n),
                Qualified(n) => self.emit_qualified_name(n),
            }
        } else if self.target == EmitTarget::DECLARATION {
            use ast::EntityNameKind::*;
            match &node.kind {
                Ident(n) => self.emit_ident(n),
                Qualified(q) => {
                    self.emit_entity_name(q.left);
                    self.dts.print().p_token(TokenKind::Dot);
                    self.emit_ident(q.right);
                }
            }
        } else {
            use ast::EntityNameKind::*;
            match &node.kind {
                Ident(n) => self.emit_ident(n),
                Qualified(q) => {
                    self.emit_entity_name(q.left);
                    self.emit_token(TokenKind::Dot);
                    if let Some(dts) = self.dts() {
                        dts.print().p_token(TokenKind::Dot);
                    }
                    self.emit_ident(q.right);
                }
            }
        }
    }
    fn emit_ty(&mut self, node: &'cx ast::Ty<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.walk_ty(node)
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty(node));
        }
    }
    fn emit_refer_ty(&mut self, n: &'cx ast::ReferTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_entity_name(n.name);
            self.emit_type_arguments(n.ty_args);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_refer_ty(n));
        }
    }
    fn emit_array_ty(&mut self, node: &'cx ast::ArrayTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_ty(node.ele);
            self.dts.print().p_token(TokenKind::LBracket);
            self.dts.print().p_token(TokenKind::RBracket);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_array_ty(node));
        }
    }
    fn emit_indexed_access_ty(&mut self, node: &'cx ast::IndexedAccessTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_ty(node.ty);
            self.dts.print().p_token(TokenKind::LBracket);
            self.emit_ty(node.index_ty);
            self.dts.print().p_token(TokenKind::RBracket);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_indexed_access_ty(node)
            });
        }
    }
    fn emit_fn_ty(&mut self, node: &'cx ast::FnTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_type_parameters(node.ty_params);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::EqGreat);
            self.dts.print().p_whitespace();
            self.emit_ty(node.ty);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_fn_ty(node));
        }
    }
    fn emit_ctor_ty(&mut self, node: &'cx ast::CtorTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::New);
            self.dts.print().p_whitespace();
            self.emit_type_parameters(node.ty_params);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);
            self.dts.print().p_token(TokenKind::EqGreat);
            self.dts.print().p_whitespace();
            self.emit_ty(node.ty);
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_ctor_ty(node));
        }
    }
    fn emit_object_lit_ty(&mut self, node: &'cx ast::ObjectLitTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LBrace);
            if !node.members.is_empty() {
                self.dts.increment_indent();
                self.dts.print().p_newline();
                self.emit_list_indexed(
                    node.members,
                    |this, elem, _| {
                        use ast::ObjectTyMemberKind::*;
                        match elem.kind {
                            IndexSig(n) => this.emit_index_sig_decl(n),
                            Prop(n) => this.emit_prop_signature(n),
                            Method(n) => this.emit_method_signature(n),
                            CallSig(n) => this.emit_call_sig_decl(n),
                            CtorSig(n) => this.emit_ctor_sig_decl(n),
                            Setter(n) => this.emit_setter_decl(n),
                            Getter(n) => this.emit_getter_decl(n),
                        }
                    },
                    |this, _, _| {
                        this.dts.print().p_newline();
                    },
                );
                self.dts.decrement_indent();
                self.dts.print().p_newline();
            }
            self.dts.print().p_token(TokenKind::RBrace);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_object_lit_ty(node)
            });
        }
    }
    fn emit_lit_ty(&mut self, node: &'cx ast::LitTy) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            use ast::LitTyKind::*;
            match &node.kind {
                Void => self.dts.print().p_token(TokenKind::Void),
                Null => self.dts.print().p_token(TokenKind::Null),
                True => self.dts.print().p_token(TokenKind::True),
                False => self.dts.print().p_token(TokenKind::False),
                Undefined => self.dts.print().p_token(TokenKind::Undefined),
                Num(num) => self.dts.print().p(&num.to_string()),
                String(atom) => self.emit_string_literal(*atom),
                BigInt { neg, val } => {
                    if *neg {
                        self.dts.print().p_token(TokenKind::Minus);
                    }
                    self.dts.emit_atom(self.resolver.atoms(), *val);
                    self.dts.print().p("n")
                }
            }
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_lit_ty(node));
        }
    }
    fn emit_named_tuple_ty(&mut self, node: &'cx ast::NamedTupleTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.walk_named_tuple_ty(node)
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_named_tuple_ty(node)
            });
        }
    }
    fn emit_tuple_ty(&mut self, node: &'cx ast::TupleTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LBracket);
            self.emit_list_indexed(
                node.tys,
                |this, item, _| {
                    this.emit_ty(item);
                },
                |this, _, _| {
                    this.dts.print().p_token(TokenKind::Comma);
                    this.dts.print().p_whitespace();
                },
            );
            self.dts.print().p_token(TokenKind::RBracket);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_tuple_ty(node));
        }
    }
    fn emit_rest_ty(&mut self, node: &'cx ast::RestTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::DotDotDot);
            self.emit_ty(node.ty);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_rest_ty(node));
        }
    }
    fn emit_cond_ty(&mut self, node: &'cx ast::CondTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_ty(node.check_ty);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::Extends);
            self.dts.print().p_whitespace();
            self.emit_ty(node.extends_ty);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::Question);
            self.dts.print().p_whitespace();
            self.emit_ty(node.true_ty);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            self.emit_ty(node.false_ty);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_cond_ty(node));
        }
    }
    fn emit_union_ty(&mut self, n: &'cx ast::UnionTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_list_indexed(
                n.tys,
                |this, item, _| {
                    this.emit_ty(item);
                },
                |this, _, _| {
                    this.dts.print().p_whitespace();
                    this.dts.print().p_token(TokenKind::Pipe);
                    this.dts.print().p_whitespace();
                },
            );
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_union_ty(n));
        }
    }
    fn emit_intersection_ty(&mut self, node: &'cx ast::IntersectionTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_list_indexed(
                node.tys,
                |this, item, _| {
                    this.emit_ty(item);
                },
                |this, _, _| {
                    this.dts.print().p_whitespace();
                    this.dts.print().p_token(TokenKind::Amp);
                    this.dts.print().p_whitespace();
                },
            );
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_intersection_ty(node)
            });
        }
    }
    fn emit_typeof_ty(&mut self, node: &'cx ast::TypeofTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::Typeof);
            self.dts.print().p_whitespace();
            self.emit_entity_name(node.name);
            self.emit_type_arguments(node.ty_args);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_typeof_ty(node));
        }
    }
    fn emit_mapped_ty(&mut self, node: &'cx ast::MappedTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LBrace);
            self.dts.print().p_newline();
            self.dts.print().p_token(TokenKind::LBracket);
            self.emit_ident(node.ty_param.name);
            if let Some(constraint) = node.ty_param.constraint {
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::In);
                self.dts.print().p_whitespace();
                self.emit_ty(constraint);
            }
            if let Some(name_ty) = node.name_ty {
                self.emit_ty(name_ty);
            }
            self.dts.print().p_token(TokenKind::RBracket);
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            if let Some(ty) = node.ty {
                self.emit_ty(ty);
            }
            self.dts.print().p_newline();
            self.dts.print().p_token(TokenKind::RBrace);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_mapped_ty(node));
        }
    }
    fn emit_ty_op_ty(&mut self, node: &'cx ast::TypeOp<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            match node.op {
                ast::TyOpKind::Keyof => self.dts.print().p_token(TokenKind::Keyof),
                ast::TyOpKind::Readonly => self.dts.print().p_token(TokenKind::Readonly),
                ast::TyOpKind::Unique => self.dts.print().p_token(TokenKind::Unique),
            }
            self.dts.print().p_whitespace();
            self.emit_ty(node.ty);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty_op_ty(node));
        }
    }
    fn emit_ty_param(&mut self, node: &'cx ast::TyParam<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_ident(node.name);
            if let Some(constraint) = node.constraint {
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Extends);
                self.dts.print().p_whitespace();
                self.emit_ty(constraint);
            }
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_ty_param(node));
        }
    }
    fn emit_pred_ty(&mut self, node: &'cx bolt_ts_ast::PredTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            if node.asserts.is_some() {
                self.dts.print().p("asserts");
                self.dts.print().p_whitespace();
            }
            match node.name {
                ast::PredTyName::Ident(ident) => self.emit_ident(ident),
                ast::PredTyName::This(_) => {
                    self.dts.print().p("this");
                }
            }
            self.dts.print().p_whitespace();
            self.dts.print().p("is");
            self.dts.print().p_whitespace();
            if let Some(ty) = node.ty {
                self.emit_ty(ty);
            }
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_pred_ty(node));
        }
    }
    fn emit_paren_ty(&mut self, node: &'cx ast::ParenTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_ty(node.ty);
            self.dts.print().p_token(TokenKind::RParen);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_paren_ty(node));
        }
    }
    fn emit_infer_ty(&mut self, node: &'cx ast::InferTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::Infer);
            self.dts.print().p_whitespace();
            self.emit_ident(node.ty_param.name);
            if let Some(constraint) = node.ty_param.constraint {
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Extends);
                self.dts.print().p_whitespace();
                self.emit_ty(constraint);
            }
            if let Some(default) = node.ty_param.default {
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Eq);
                self.dts.print().p_whitespace();
                self.emit_ty(default);
            }
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_infer_ty(node));
        }
    }
    fn emit_intrinsic_ty(&mut self, node: &'cx ast::IntrinsicTy) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.walk_intrinsic_ty(node)
        }
    }
    fn emit_nullable_ty(&mut self, node: &'cx ast::NullableTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.walk_nullable_ty(node)
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_nullable_ty(node));
        }
    }
    fn emit_template_lit_ty(&mut self, node: &'cx ast::TemplateLitTy<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            emit_template_head_ty(self, node.head);
            for span in node.spans {
                self.emit_template_span_ty(span)
            }
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_template_lit_ty(node)
            });
        }
    }
    fn emit_this_ty(&mut self, node: &'cx ast::ThisTy) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.walk_this_ty(node)
        }
    }
    fn emit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
            self.dts.print().p_token(TokenKind::Interface);
            self.dts.print().p_whitespace();
            self.dts.emit_atom(self.resolver.atoms(), node.name.name);
            self.emit_type_parameters(node.ty_params);
            if let Some(extends) = node.extends {
                self.dts.print().p_whitespace();
                self.dts.print().p_token(TokenKind::Extends);
                self.dts.print().p_whitespace();
                self.emit_list(
                    extends.list,
                    |this, item| {
                        this.emit_refer_ty(item);
                    },
                    |this, _| {
                        this.dts.print().p_token(TokenKind::Comma);
                        this.dts.print().p_whitespace();
                    },
                );
            }
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::LBrace);
            if !node.members.is_empty() {
                self.dts.increment_indent();
                self.dts.print().p_newline();
                self.emit_list_indexed(
                    node.members,
                    |this, elem, _| {
                        this.walk_object_ty_member(elem);
                    },
                    |this, _, _| {
                        this.dts.print().p_newline();
                    },
                );
                self.dts.decrement_indent();
                self.dts.print().p_newline();
            }
            self.dts.print().p_token(TokenKind::RBrace);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_interface_decl(node)
            });
        }
    }
    fn emit_type_alias_decl(&mut self, node: &'cx ast::TypeAliasDecl<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::Type);
            self.dts.print().p_whitespace();
            self.dts.emit_atom(self.resolver.atoms(), node.name.name);
            self.emit_type_parameters(node.ty_params);
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::Eq);
            self.dts.print().p_whitespace();
            self.emit_ty(node.ty);
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_type_alias_decl(node)
            });
        }
    }
    fn emit_index_sig_decl(&mut self, node: &'cx ast::IndexSigDecl<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LBracket);
            self.emit_binding(node.key);
            self.dts.print().p_token(TokenKind::Colon);
            self.emit_ty(node.key_ty);
            self.dts.print().p_token(TokenKind::RBracket);
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            self.emit_ty(node.ty);
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_index_sig_decl(node)
            });
        }
    }
    fn emit_prop_signature(&mut self, node: &'cx ast::PropSignature<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_prop_name(node.name);
            if node.question.is_some() {
                self.dts.print().p_token(TokenKind::Question);
            }
            if let Some(ty) = node.ty {
                self.dts.print().p_token(TokenKind::Colon);
                self.dts.print().p_whitespace();
                self.emit_ty(ty);
            }
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_prop_signature(node)
            });
        }
    }
    fn emit_method_signature(&mut self, node: &'cx ast::MethodSignature<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_prop_name(node.name);
            if node.question.is_some() {
                self.dts.print().p_token(TokenKind::Question);
            }
            self.emit_type_parameters(node.ty_params);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);

            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            self.emit_ret_ty(node.ty);
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_method_signature(node)
            });
        }
    }
    fn emit_call_sig_decl(&mut self, node: &'cx ast::CallSigDecl<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            self.emit_ret_ty(node.ty);
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_call_sig_decl(node)
            });
        }
    }
    fn emit_ctor_sig_decl(&mut self, node: &'cx ast::CtorSigDecl<'cx>) {
        if self.target == EmitTarget::JS {
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::New);
            self.dts.print().p_whitespace();
            self.emit_type_parameters(node.ty_params);
            self.dts.print().p_token(TokenKind::LParen);
            self.emit_parameters(node.params);
            self.dts.print().p_token(TokenKind::RParen);
            self.dts.print().p_token(TokenKind::Colon);
            self.dts.print().p_whitespace();
            self.emit_ret_ty(node.ty);
            self.dts.print().p_token(TokenKind::Semi);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_ctor_sig_decl(node)
            });
        }
    }
    fn emit_module_block(&mut self, node: &'cx ast::ModuleBlock<'cx>) {
        if self.target == EmitTarget::JS {
            self.walk_module_block(node)
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_whitespace();
            self.dts.print().p_token(TokenKind::LBrace);
            let saved_flags = self.flags;
            // TODO: condition for `STRIP_EXPORT_MODIFIER`
            self.flags
                .insert(EmitDeclarationFlags::STRIP_EXPORT_MODIFIER);
            self.flags.remove(EmitDeclarationFlags::NEED_DECLARE);
            self.dts.increment_indent();
            self.dts.print().p_newline();
            self.emit_list_indexed(
                node.stmts,
                |this, stmt, _| this.emit_stmt(stmt),
                |this, _, _| this.dts.print().p_newline(),
            );
            self.dts.decrement_indent();
            self.dts.print().p_newline();
            self.flags = saved_flags;
            self.dts.print().p_token(TokenKind::RBrace);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_module_block(node));
        }
    }
    fn emit_array_pat(&mut self, node: &'cx ast::ArrayPat<'cx>) {
        if self.target == EmitTarget::JS {
            self.walk_array_pat(node)
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LBracket);
            self.emit_list_indexed(
                node.elems,
                |this, item, _| match item.kind {
                    ast::ArrayBindingElemKind::Omit(_) => {}
                    ast::ArrayBindingElemKind::Binding(item) => {
                        this.emit_array_binding(item);
                    }
                },
                |this, _, _| {
                    this.dts.print().p_token(TokenKind::Comma);
                    this.dts.print().p_whitespace();
                },
            );
            self.dts.print().p_token(TokenKind::RBracket);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_array_pat(node));
        }
    }
    fn emit_object_pat(&mut self, n: &'cx ast::ObjectPat<'cx>) {
        if self.target == EmitTarget::JS {
            self.walk_object_pat(n)
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LBrace);
            self.emit_list_indexed(
                n.elems,
                |this, item, _| this.emit_object_binding_elem(item),
                |this, _, _| {
                    this.dts.print().p_token(TokenKind::Comma);
                    this.dts.print().p_whitespace();
                },
            );
            self.dts.print().p_token(TokenKind::RBrace);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| this.emit_object_pat(n));
        }
    }
    fn emit_computed_prop_name(&mut self, node: &'cx ast::ComputedPropName<'cx>) {
        if self.target == EmitTarget::JS {
            self.walk_computed_prop_name(node)
        } else if self.target == EmitTarget::DECLARATION {
            self.dts.print().p_token(TokenKind::LBracket);
            self.emit_expr(node.expr);
            self.dts.print().p_token(TokenKind::RBracket);
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_computed_prop_name(node)
            });
        }
    }
    fn emit_template_span_ty(&mut self, node: &'cx ast::TemplateSpanTy<'cx>) {
        if self.target == EmitTarget::JS {
            self.walk_template_span_ty(node)
        } else if self.target == EmitTarget::DECLARATION {
            self.emit_ty(node.ty);
            self.dts.emit_atom(self.resolver.atoms(), node.text);
            if node.is_tail {
                self.dts.print().p_token(TokenKind::RBrace);
                self.dts.print().p_token(TokenKind::Backtick);
            } else {
                self.dts.print().p_token(TokenKind::RBrace);
                self.dts.print().p_token(TokenKind::Dollar);
                self.dts.print().p_token(TokenKind::LBrace);
            }
        } else {
            self.with_target(EmitTarget::DECLARATION, |this| {
                this.emit_template_span_ty(node)
            });
        }
    }
    fn emit_class_extends_clause(&mut self, n: &'cx ast::ClassExtendsClause<'cx>) {
        self.walk_class_extends_clause(n);
    }
    fn emit_class_implements_clause(&mut self, n: &'cx ast::ClassImplementsClause<'cx>) {
        self.walk_class_implements_clause(n);
    }
    fn emit_class_semi_elem(&mut self, _n: &'cx ast::ClassSemiElem) {
        self.walk_class_semi_elem(_n);
    }
    fn emit_debugger_stmt(&mut self, _n: &'cx ast::DebuggerStmt) {
        self.walk_debugger_stmt(_n);
    }
    fn emit_empty_stmt(&mut self, _n: &'cx ast::EmptyStmt) {
        self.walk_empty_stmt(_n);
    }
    fn emit_expr_stmt(&mut self, n: &'cx ast::ExprStmt<'cx>) {
        self.walk_expr_stmt(n);
    }
    fn emit_glob_export(&mut self, n: &'cx ast::GlobExport<'cx>) {
        self.walk_glob_export(n);
    }
    fn emit_import_expression(&mut self, _n: &'cx ast::ImportExpression) {
        self.walk_import_expression(_n);
    }
    fn emit_import_type(&mut self, n: &'cx ast::ImportType<'cx>) {
        self.walk_import_type(n);
    }
    fn emit_jsx_closing_elem(&mut self, n: &'cx ast::JsxClosingElem<'cx>) {
        self.walk_jsx_closing_elem(n);
    }
    fn emit_jsx_closing_frag(&mut self, _n: &'cx ast::JsxClosingFrag) {
        self.walk_jsx_closing_frag(_n);
    }
    fn emit_jsx_named_attr(&mut self, n: &'cx ast::JsxNamedAttr<'cx>) {
        self.walk_jsx_named_attr(n);
    }
    fn emit_jsx_opening_elem(&mut self, n: &'cx ast::JsxOpeningElem<'cx>) {
        self.walk_jsx_opening_elem(n);
    }
    fn emit_jsx_opening_frag(&mut self, _n: &'cx ast::JsxOpeningFrag) {
        self.walk_jsx_opening_frag(_n);
    }
    fn emit_jsx_spread_attr(&mut self, n: &'cx ast::JsxSpreadAttr<'cx>) {
        self.walk_jsx_spread_attr(n);
    }
    fn emit_jsx_text(&mut self, _n: &'cx ast::JsxText) {
        self.walk_jsx_text(_n);
    }
    fn emit_optional_ty(&mut self, n: &'cx ast::OptionalTy<'cx>) {
        self.walk_optional_ty(n);
    }
    fn emit_specs_export(&mut self, n: &'cx ast::SpecsExport<'cx>) {
        self.walk_specs_export(n);
    }
    fn emit_spread_assignment(&mut self, n: &'cx ast::SpreadAssignment<'cx>) {
        self.walk_spread_assignment(n);
    }
    fn emit_template_head(&mut self, _n: &'cx ast::TemplateHead) {
        self.walk_template_head(_n);
    }
    fn emit_template_span(&mut self, n: &'cx ast::TemplateSpan<'cx>) {
        self.walk_template_span(n);
    }
    fn walk_case_block(&mut self, n: &'cx ast::CaseBlock<'cx>) {
        for clause in n.clauses {
            match clause {
                ast::CaseOrDefaultClause::Case(n) => self.emit_case_clause(n),
                ast::CaseOrDefaultClause::Default(n) => self.emit_default_clause(n),
            }
        }
    }
    fn walk_continue_stmt(&mut self, n: &'cx ast::ContinueStmt<'cx>) {
        if let Some(label) = n.label {
            self.emit_ident(label)
        }
    }
    fn walk_object_lit(&mut self, n: &'cx ast::ObjectLit<'cx>) {
        for member in n.members {
            use ast::ObjectMemberKind::*;
            match member.kind {
                PropAssignment(n) => self.emit_object_prop_assignment(n),
                Method(n) => self.emit_object_method_member(n),
                Shorthand(n) => self.emit_object_shorthand_member(n),
                SpreadAssignment(n) => self.emit_spread_assignment(n),
                Getter(n) => self.emit_getter_decl(n),
                Setter(n) => self.emit_setter_decl(n),
            };
        }
    }
    fn walk_object_shorthand_member(&mut self, n: &'cx ast::ObjectShorthandMember<'cx>) {
        self.emit_ident(n.name);
        self.walk_optional_expr(n.object_assignment_initializer)
    }
    fn walk_jsx_elem(&mut self, n: &'cx ast::JsxElem<'cx>) {
        self.emit_jsx_opening_elem(n.opening_elem);
        for child in n.children {
            self.walk_jsx_child(*child);
        }
        self.emit_jsx_closing_elem(n.closing_elem)
    }
    fn walk_jsx_opening_frag(&mut self, _n: &'cx ast::JsxOpeningFrag) {}
    fn walk_getter_decl(&mut self, n: &'cx ast::GetterDecl<'cx>) {
        self.emit_prop_name(n.name);
        self.walk_option_ty(n.ty);
        self.walk_optional_block_stmt(n.body)
    }
    fn walk_import_expression(&mut self, _n: &'cx ast::ImportExpression) {}
    fn walk_catch_clause(&mut self, n: &'cx ast::CatchClause<'cx>) {
        if let Some(var) = n.var {
            self.emit_var_decl(var);
        }
        self.emit_block_stmt(n.block)
    }
    fn walk_arrow_fn_expr(&mut self, n: &'cx ast::ArrowFnExpr<'cx>) {
        self.walk_call_like_signature(n.ty_params, n.params, n.ty);
        match n.body {
            ast::ArrowFnExprBody::Expr(expr) => self.emit_expr(expr),
            ast::ArrowFnExprBody::Block(block) => self.emit_block_stmt(block),
        }
    }
    fn walk_jsx_named_attr(&mut self, n: &'cx ast::JsxNamedAttr<'cx>) {
        self.walk_jsx_attr_name(n.name.clone());
        if let Some(init) = n.init {
            self.walk_jsx_attr_value(init);
        }
    }
    fn walk_try_stmt(&mut self, n: &'cx ast::TryStmt<'cx>) {
        self.emit_block_stmt(n.try_block);
        if let Some(catch) = n.catch_clause {
            self.emit_catch_clause(catch);
        }
        if let Some(finally) = n.finally_block {
            self.emit_block_stmt(finally);
        }
    }
    fn walk_export_named_spec(&mut self, n: &'cx ast::ExportNamedSpec<'cx>) {
        self.emit_module_export_name(n.name);
        self.emit_module_export_name(n.prop_name)
    }
    fn walk_jsx_closing_frag(&mut self, _n: &'cx ast::JsxClosingFrag) {}
    fn walk_spread_assignment(&mut self, n: &'cx ast::SpreadAssignment<'cx>) {
        self.emit_expr(n.expr)
    }
    fn walk_template_expr(&mut self, n: &'cx ast::TemplateExpr<'cx>) {
        self.emit_template_head(n.head);
        for span in n.spans {
            self.emit_template_span(span);
        }
    }
    fn walk_optional_ty(&mut self, n: &'cx ast::OptionalTy<'cx>) {
        self.emit_ty(n.ty)
    }
    fn walk_bin_expr(&mut self, n: &'cx ast::BinExpr<'cx>) {
        self.emit_expr(n.left);
        self.emit_expr(n.right)
    }
    fn walk_jsx_text(&mut self, _n: &'cx ast::JsxText) {}
    fn walk_switch_stmt(&mut self, n: &'cx ast::SwitchStmt<'cx>) {
        self.emit_expr(n.expr);
        self.emit_case_block(n.case_block)
    }
    fn walk_class_implements_clause(&mut self, n: &'cx ast::ClassImplementsClause<'cx>) {
        self.walk_refer_tys(n.list)
    }
    fn walk_jsx_tag_name(&mut self, name: ast::JsxTagName<'cx>) {
        use ast::JsxTagName::*;
        match name {
            Ident(n) => self.emit_ident(n),
            This(n) => self.emit_this_expr(n),
            Ns(n) => self.emit_jsx_ns_name(n),
            PropAccess(n) => self.emit_prop_access_expr(n),
        }
    }
    fn walk_fn_expr(&mut self, n: &'cx ast::FnExpr<'cx>) {
        if let Some(name) = n.name {
            self.emit_ident(name);
        }
        self.walk_call_like_signature(n.ty_params, n.params, n.ty);
        self.emit_block_stmt(n.body)
    }
    fn walk_optional_block_stmt(&mut self, body: Option<&'cx ast::BlockStmt<'cx>>) {
        if let Some(body) = body {
            self.emit_block_stmt(body);
        }
    }
    fn walk_class_elem(&mut self, elem: &'cx ast::ClassElem<'cx>) {
        use ast::ClassElemKind::*;
        match elem.kind {
            Ctor(n) => self.emit_class_ctor(n),
            Prop(n) => self.emit_class_prop_elem(n),
            Method(n) => self.emit_class_method_elem(n),
            IndexSig(n) => self.emit_index_sig_decl(n),
            Getter(n) => self.emit_getter_decl(n),
            Setter(n) => self.emit_setter_decl(n),
            StaticBlockDecl(n) => self.emit_class_static_block_decl(n),
            Semi(n) => self.emit_class_semi_elem(n),
        }
    }
    fn walk_jsx_opening_elem(&mut self, n: &'cx ast::JsxOpeningElem<'cx>) {
        self.walk_jsx_tag_name(n.tag_name);
        self.walk_type_arguments(n.ty_args);
        self.walk_jsx_attrs(n.attrs)
    }
    fn walk_ty(&mut self, ty: &'cx ast::Ty<'cx>) {
        use ast::TyKind::*;
        match ty.kind {
            Refer(n) => self.emit_refer_ty(n),
            Array(n) => self.emit_array_ty(n),
            IndexedAccess(n) => self.emit_indexed_access_ty(n),
            Fn(n) => self.emit_fn_ty(n),
            Ctor(n) => self.emit_ctor_ty(n),
            ObjectLit(n) => self.emit_object_lit_ty(n),
            Lit(n) => self.emit_lit_ty(n),
            NamedTuple(n) => self.emit_named_tuple_ty(n),
            Tuple(n) => self.emit_tuple_ty(n),
            Rest(n) => self.emit_rest_ty(n),
            Cond(n) => self.emit_cond_ty(n),
            Union(n) => self.emit_union_ty(n),
            Intersection(n) => self.emit_intersection_ty(n),
            Typeof(n) => self.emit_typeof_ty(n),
            Mapped(n) => self.emit_mapped_ty(n),
            TypeOp(n) => self.emit_ty_op_ty(n),
            Pred(n) => self.emit_pred_ty(n),
            Paren(n) => self.emit_paren_ty(n),
            Infer(n) => self.emit_infer_ty(n),
            Intrinsic(n) => self.emit_intrinsic_ty(n),
            Nullable(n) => self.emit_nullable_ty(n),
            TemplateLit(n) => self.emit_template_lit_ty(n),
            This(n) => self.emit_this_ty(n),
            Import(n) => self.emit_import_type(n),
            Optional(n) => self.emit_optional_ty(n),
        }
    }
    fn walk_exprs(&mut self, exprs: &'cx [&'cx ast::Expr<'cx>]) {
        for expr in exprs {
            self.emit_expr(expr);
        }
    }
    fn walk_param_decls(&mut self, params: ast::ParamsDecl<'cx>) {
        for param in params {
            self.emit_param_decl(param);
        }
    }
    fn walk_export_decl(&mut self, n: &'cx ast::ExportDecl<'cx>) {
        self.walk_export_clause(n.clause)
    }
    fn walk_big_int_lit(&mut self, _n: &'cx ast::BigIntLit) {}
    fn walk_import_type(&mut self, n: &'cx ast::ImportType<'cx>) {
        self.emit_ty(n.argument);
        if let Some(qualifier) = n.qualifier {
            self.emit_entity_name(qualifier);
        }
        self.walk_type_arguments(n.type_arguments)
    }
    fn walk_expr_stmt(&mut self, n: &'cx ast::ExprStmt<'cx>) {
        self.emit_expr(n.expr)
    }
    fn walk_ns_export(&mut self, n: &'cx ast::NsExport<'cx>) {
        self.emit_module_export_name(n.name);
        self.emit_string_lit(n.module)
    }
    fn walk_jsx_child(&mut self, child: ast::JsxChild<'cx>) {
        use ast::JsxChild::*;
        match child {
            Text(n) => self.emit_jsx_text(n),
            Expr(n) => self.emit_jsx_expr(n),
            Elem(n) => self.emit_jsx_elem(n),
            SelfClosingEle(n) => self.emit_jsx_self_closing_elem(n),
            Frag(n) => self.emit_jsx_frag(n),
        }
    }
    fn walk_jsx_spread_attr(&mut self, n: &'cx ast::JsxSpreadAttr<'cx>) {
        self.emit_expr(n.expr)
    }
    fn walk_module_block(&mut self, n: &'cx ast::ModuleBlock<'cx>) {
        self.walk_stmts(n.stmts)
    }
    fn walk_export_clause(&mut self, clause: &'cx ast::ExportClause<'cx>) {
        match clause.kind {
            ast::ExportClauseKind::Glob(n) => self.emit_glob_export(n),
            ast::ExportClauseKind::Ns(n) => self.emit_ns_export(n),
            ast::ExportClauseKind::Specs(n) => self.emit_specs_export(n),
        }
    }
    fn walk_named_tuple_ty(&mut self, n: &'cx ast::NamedTupleTy<'cx>) {
        self.emit_ident(n.name);
        self.emit_ty(n.ty)
    }
    fn walk_this_ty(&mut self, _n: &'cx ast::ThisTy) {}
    fn walk_binding(&mut self, node: &'cx ast::Binding<'cx>) {
        use ast::BindingKind::*;
        match node.kind {
            Ident(n) => self.emit_ident(n),
            ObjectPat(n) => self.emit_object_pat(n),
            ArrayPat(n) => self.emit_array_pat(n),
        }
    }
    fn walk_jsx_attr_value(&mut self, value: ast::JsxAttrValue<'cx>) {
        use ast::JsxAttrValue::*;
        match value {
            StringLit(n) => self.emit_string_lit(n),
            Expr(n) => self.emit_jsx_expr(n),
            Ele(n) => self.emit_jsx_elem(n),
            SelfClosingEle(n) => self.emit_jsx_self_closing_elem(n),
            Frag(n) => self.emit_jsx_frag(n),
        }
    }
    fn walk_type_parameters(&mut self, ty_params: Option<ast::TyParams<'cx>>) {
        if let Some(ty_params) = ty_params {
            for ty_param in ty_params {
                self.emit_ty_param(ty_param);
            }
        }
    }
    fn walk_expr_with_ty_args(&mut self, n: &'cx ast::ExprWithTyArgs<'cx>) {
        self.emit_expr(n.expr);
        self.walk_type_arguments(n.ty_args)
    }
    fn walk_array_pat(&mut self, pat: &'cx ast::ArrayPat<'cx>) {
        for elem in pat.elems {
            match elem.kind {
                ast::ArrayBindingElemKind::Omit(_) => {}
                ast::ArrayBindingElemKind::Binding(binding) => {
                    self.emit_array_binding(binding);
                }
            }
        }
    }
    fn walk_optional_exprs(&mut self, exprs: Option<&'cx [&'cx ast::Expr<'cx>]>) {
        if let Some(exprs) = exprs {
            self.walk_exprs(exprs)
        }
    }
    fn walk_jsx_closing_elem(&mut self, n: &'cx ast::JsxClosingElem<'cx>) {
        self.walk_jsx_tag_name(n.tag_name)
    }
    fn walk_object_method_member(&mut self, n: &'cx ast::ObjectMethodMember<'cx>) {
        self.emit_prop_name(n.name);
        self.walk_call_like_signature(n.ty_params, n.params, n.ty);
        self.emit_block_stmt(n.body)
    }
    fn walk_array_lit(&mut self, n: &'cx ast::ArrayLit<'cx>) {
        self.walk_exprs(n.elems)
    }
    fn walk_object_ty_member(&mut self, member: &'cx ast::ObjectTyMember<'cx>) {
        use ast::ObjectTyMemberKind::*;
        match member.kind {
            IndexSig(n) => self.emit_index_sig_decl(n),
            Prop(n) => self.emit_prop_signature(n),
            Method(n) => self.emit_method_signature(n),
            CallSig(n) => self.emit_call_sig_decl(n),
            CtorSig(n) => self.emit_ctor_sig_decl(n),
            Setter(n) => self.emit_setter_decl(n),
            Getter(n) => self.emit_getter_decl(n),
        }
    }
    fn walk_class_expr(&mut self, n: &'cx ast::ClassExpr<'cx>) {
        if let Some(name) = n.name {
            self.emit_ident(name);
        }
        self.emit_type_parameters(n.ty_params);
        if let Some(extends) = n.extends {
            self.emit_class_extends_clause(extends);
        }
        if let Some(implements) = n.implements {
            self.emit_class_implements_clause(implements);
        }
        for ele in n.elems.list {
            self.emit_class_elem(ele);
        }
    }
    fn walk_no_substitution_template_lit(&mut self, _n: &'cx ast::NoSubstitutionTemplateLit) {}
    fn walk_paren_expr(&mut self, n: &'cx ast::ParenExpr<'cx>) {
        self.emit_expr(n.expr)
    }
    fn walk_export_shorthand_spec(&mut self, n: &'cx ast::ExportShorthandSpec<'cx>) {
        self.emit_ident(n.name)
    }
    fn walk_type_arguments(&mut self, ty_args: Option<&'cx ast::Tys<'cx>>) {
        if let Some(ty_args) = ty_args {
            self.walk_tys(ty_args.list)
        }
    }
    fn walk_nullable_ty(&mut self, n: &'cx ast::NullableTy<'cx>) {
        self.emit_ty(n.ty)
    }
    fn walk_refer_tys(&mut self, tys: &'cx [&'cx ast::ReferTy<'cx>]) {
        for ty in tys {
            self.emit_refer_ty(ty);
        }
    }
    fn walk_do_while_stmt(&mut self, n: &'cx ast::DoWhileStmt<'cx>) {
        self.emit_stmt(n.stmt);
        self.emit_expr(n.expr)
    }
    fn walk_private_ident(&mut self, _n: &'cx ast::PrivateIdent) {}
    fn walk_ret_stmt(&mut self, n: &'cx ast::RetStmt<'cx>) {
        self.walk_optional_expr(n.expr)
    }
    fn walk_jsx_attr_name(&mut self, name: ast::JsxAttrName<'cx>) {
        use ast::JsxAttrName::*;
        match name {
            Ident(n) => self.emit_ident(n),
            Ns(n) => self.emit_jsx_ns_name(n),
        }
    }
    fn walk_enum_member_name_kind(&mut self, name: &ast::EnumMemberNameKind<'cx>) {
        use ast::EnumMemberNameKind::*;
        match name {
            Ident(ident) => self.emit_ident(ident),
            StringLit { raw, .. } => self.emit_string_lit(raw),
        }
    }
    fn walk_module_export_name(&mut self, name: &'cx ast::ModuleExportName<'cx>) {
        match name.kind {
            ast::ModuleExportNameKind::Ident(n) => self.emit_ident(n),
            ast::ModuleExportNameKind::StringLit(n) => self.emit_string_lit(n),
        }
    }
    fn walk_default_clause(&mut self, n: &'cx ast::DefaultClause<'cx>) {
        self.walk_stmts(n.stmts)
    }
    fn walk_module_name(&mut self, name: ast::ModuleName<'cx>) {
        match name {
            ast::ModuleName::Ident(ident) => self.emit_ident(ident),
            ast::ModuleName::StringLit(lit) => self.emit_string_lit(lit),
        }
    }
    fn walk_jsx_attrs(&mut self, attrs: ast::JsxAttrs<'cx>) {
        for attr in attrs {
            self.walk_jsx_attr(attr);
        }
    }
    fn walk_class_static_block_decl(&mut self, n: &'cx ast::ClassStaticBlockDecl<'cx>) {
        self.emit_block_stmt(n.body)
    }
    fn walk_jsx_ns_name(&mut self, n: &'cx ast::JsxNsName<'cx>) {
        self.emit_ident(n.ns);
        self.emit_ident(n.name)
    }
    fn walk_object_pat(&mut self, pat: &'cx ast::ObjectPat<'cx>) {
        for elem in pat.elems {
            self.emit_object_binding_elem(elem);
        }
    }
    fn walk_if_stmt(&mut self, n: &'cx ast::IfStmt<'cx>) {
        self.emit_expr(n.expr);
        self.emit_stmt(n.then);
        if let Some(else_then) = n.else_then {
            self.emit_stmt(else_then);
        }
    }
    fn walk_stmts(&mut self, stmts: ast::Stmts<'cx>) {
        for stmt in stmts {
            self.emit_stmt(stmt);
        }
    }
    fn walk_this_expr(&mut self, _n: &'cx ast::ThisExpr) {}
    fn walk_option_ty(&mut self, ty: Option<&'cx ast::Ty<'cx>>) {
        if let Some(ty) = ty {
            self.emit_ty(ty);
        }
    }
    fn walk_intrinsic_ty(&mut self, _n: &'cx ast::IntrinsicTy) {}
    fn walk_template_head(&mut self, _n: &'cx ast::TemplateHead) {}
    fn walk_template_span(&mut self, n: &'cx ast::TemplateSpan<'cx>) {
        self.emit_expr(n.expr)
    }
    fn walk_empty_stmt(&mut self, _n: &'cx ast::EmptyStmt) {}
    fn walk_labeled_stmt(&mut self, n: &'cx ast::LabeledStmt<'cx>) {
        self.emit_ident(n.label);
        self.emit_stmt(n.stmt)
    }
    fn walk_case_clause(&mut self, n: &'cx ast::CaseClause<'cx>) {
        self.emit_expr(n.expr);
        self.walk_stmts(n.stmts)
    }
    fn walk_block_stmt(&mut self, n: &'cx ast::BlockStmt<'cx>) {
        self.walk_stmts(n.stmts)
    }
    fn walk_stmt(&mut self, stmt: &'cx ast::Stmt) {
        use ast::StmtKind::*;
        match stmt.kind {
            Block(node) => self.emit_block_stmt(node),
            Class(node) => self.emit_class_decl(node),
            Import(node) => self.emit_import_decl(node),
            ImportEquals(node) => self.emit_import_equals_decl(node),
            Interface(node) => self.emit_interface_decl(node),
            Expr(node) => self.emit_expr_stmt(node),
            Var(node) => self.emit_var_stmt(node),
            Try(node) => self.emit_try_stmt(node),
            TypeAlias(node) => self.emit_type_alias_decl(node),
            NestedModule(node) => self.emit_nested_module_decl(node),
            BlockModule(node) => self.emit_block_module_decl(node),
            While(node) => self.emit_while_stmt(node),
            If(node) => self.emit_if_stmt(node),
            Enum(node) => self.emit_enum_decl(node),
            Fn(node) => self.emit_fn_decl(node),
            Ret(node) => self.emit_ret_stmt(node),
            Export(node) => self.emit_export_decl(node),
            ExportAssign(node) => self.emit_export_assign(node),
            Empty(node) => self.emit_empty_stmt(node),
            Throw(node) => self.emit_throw_stmt(node),
            For(node) => self.emit_for_stmt(node),
            ForOf(node) => self.emit_for_of_stmt(node),
            ForIn(node) => self.emit_for_in_stmt(node),
            Break(node) => self.emit_break_stmt(node),
            Continue(node) => self.emit_continue_stmt(node),
            Do(node) => self.emit_do_while_stmt(node),
            Debugger(node) => self.emit_debugger_stmt(node),
            Labeled(node) => self.emit_labeled_stmt(node),
            Switch(node) => self.emit_switch_stmt(node),
        }
    }
    fn walk_class_extends_clause(&mut self, n: &'cx ast::ClassExtendsClause<'cx>) {
        self.emit_expr_with_ty_args(n.expr_with_ty_args)
    }
    fn walk_tys(&mut self, tys: &'cx [&'cx ast::Ty<'cx>]) {
        for ty in tys {
            self.emit_ty(ty);
        }
    }
    fn walk_jsx_frag(&mut self, n: &'cx ast::JsxFrag<'cx>) {
        self.emit_jsx_opening_frag(n.opening_frag);
        for child in n.children {
            self.walk_jsx_child(*child);
        }
        self.emit_jsx_closing_frag(n.closing_frag)
    }
    fn walk_while_stmt(&mut self, n: &'cx ast::WhileStmt<'cx>) {
        self.emit_expr(n.expr);
        self.emit_stmt(n.stmt)
    }
    fn walk_optional_expr(&mut self, expr: Option<&'cx ast::Expr<'cx>>) {
        if let Some(expr) = expr {
            self.emit_expr(expr);
        }
    }
    fn walk_ns_import(&mut self, n: &'cx ast::NsImport<'cx>) {
        self.emit_ident(n.name)
    }
    fn walk_new_expr(&mut self, n: &'cx ast::NewExpr<'cx>) {
        self.emit_expr(n.expr);
        self.walk_type_arguments(n.ty_args);
        self.walk_optional_exprs(n.args)
    }
    fn walk_qualified_name(&mut self, n: &'cx ast::QualifiedName<'cx>) {
        self.emit_entity_name(n.left);
        self.emit_ident(n.right)
    }
    fn walk_glob_export(&mut self, n: &'cx ast::GlobExport<'cx>) {
        self.emit_string_lit(n.module)
    }
    fn walk_call_like_signature(
        &mut self,
        ty_params: Option<ast::TyParams<'cx>>,
        params: ast::ParamsDecl<'cx>,
        ty: Option<&'cx ast::Ty<'cx>>,
    ) {
        self.walk_type_parameters(ty_params);
        self.walk_param_decls(params);
        self.walk_option_ty(ty)
    }
    fn walk_break_stmt(&mut self, n: &'cx ast::BreakStmt<'cx>) {
        if let Some(label) = n.label {
            self.emit_ident(label)
        }
    }
    fn walk_jsx_attr(&mut self, attr: &ast::JsxAttr<'cx>) {
        use ast::JsxAttr::*;
        match attr {
            Spread(n) => self.emit_jsx_spread_attr(n),
            Named(n) => self.emit_jsx_named_attr(n),
        }
    }
    fn walk_debugger_stmt(&mut self, _n: &'cx ast::DebuggerStmt) {}
    fn walk_cond_expr(&mut self, n: &'cx ast::CondExpr<'cx>) {
        self.emit_expr(n.cond);
        self.emit_expr(n.when_true);
        self.emit_expr(n.when_false)
    }
    fn walk_specs_export(&mut self, n: &'cx ast::SpecsExport<'cx>) {
        for spec in n.list {
            match spec.kind {
                ast::ExportSpecKind::Shorthand(n) => self.emit_export_shorthand_spec(n),
                ast::ExportSpecKind::Named(n) => self.emit_export_named_spec(n),
            }
        }
        if let Some(module) = n.module {
            self.emit_string_lit(module);
        }
    }
    fn walk_template_span_ty(&mut self, n: &'cx ast::TemplateSpanTy<'cx>) {
        self.emit_ty(n.ty)
    }
    fn walk_object_prop_assignment(&mut self, n: &'cx ast::ObjectPropAssignment<'cx>) {
        self.emit_prop_name(n.name);
        self.emit_expr(n.init)
    }
    fn walk_reg_exp_lit(&mut self, _n: &'cx ast::RegExpLit) {}
    fn walk_computed_prop_name(&mut self, n: &'cx ast::ComputedPropName<'cx>) {
        self.emit_expr(n.expr)
    }
    fn walk_jsx_self_closing_elem(&mut self, n: &'cx ast::JsxSelfClosingElem<'cx>) {
        self.walk_jsx_tag_name(n.tag_name);
        self.walk_type_arguments(n.ty_args);
        self.walk_jsx_attrs(n.attrs)
    }
    fn walk_prop_access_expr(&mut self, n: &'cx ast::PropAccessExpr<'cx>) {
        self.emit_expr(n.expr);
        self.emit_ident(n.name)
    }
    fn walk_class_semi_elem(&mut self, _n: &'cx ast::ClassSemiElem) {}
    fn walk_throw_stmt(&mut self, n: &'cx ast::ThrowStmt<'cx>) {
        self.emit_expr(n.expr)
    }
    fn walk_jsx_expr(&mut self, n: &'cx ast::JsxExpr<'cx>) {
        self.walk_optional_expr(n.expr)
    }
}

fn get_source_text_from_source(source: &str, span: bolt_ts_span::Span) -> &str {
    let lo = span.lo() as usize;
    let hi = span.hi() as usize;
    debug_assert!(lo < hi);
    &source[lo..hi]
}

fn print_token_value(emitter: &mut Emitter, token: TokenKind, val: &str) {
    if token == TokenKind::String {
        emitter.print().p_string_literal(val);
    } else {
        emitter.print().p(val);
    }
}

fn escape_snippet_text(s: &str) -> String {
    s.replace('$', "\\$")
}

fn next_adjacent_token_on_this_line_is(origin: &[u8], mut pos: usize, t: TokenKind) -> bool {
    let t = t.as_str();
    loop {
        let Some(ch) = origin.get(pos) else {
            return false;
        };
        if is_line_break(*ch) {
            break false;
        }
        if ch.is_ascii_whitespace() {
            pos += 1;
            continue;
        }
        if origin[pos..].starts_with(t.as_bytes()) {
            break true;
        } else {
            pos += t.len();
            continue;
        }
    }
}

fn next_adjacent_token_on_this_line_is_semi(origin: &[u8], pos: usize) -> bool {
    next_adjacent_token_on_this_line_is(origin, pos, TokenKind::Semi)
}

fn emit_template_head_ty(v: &mut ModuleEmitter, node: &ast::TemplateHead) {
    v.dts.print().p_token(TokenKind::Backtick);
    v.dts.emit_atom(v.resolver.atoms(), node.text);
    v.dts.print().p_dollar_and_brace();
}

fn emit_nested_module_declaration_inner<'cx>(
    v: &mut ModuleEmitter<'cx, '_>,
    node: &'cx ast::NestedModuleDecl<'cx>,
) {
    v.dts.print().p_token(TokenKind::Dot);
    v.emit_ident(node.name);
    match node.block {
        ast::NestedModuleBlock::Nested(decl) => emit_nested_module_declaration_inner(v, decl),
        ast::NestedModuleBlock::Block(block) => v.emit_module_block(block),
    }
}
