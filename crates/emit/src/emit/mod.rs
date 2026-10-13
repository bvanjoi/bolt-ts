mod print;

use std::borrow::Cow;

use bolt_ts_ast as ast;
use bolt_ts_ast::TokenKind;
use bolt_ts_ast_visitor::{Visitor, visit_module_name};
use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_checker::{check::TyChecker, emit_resolver::EmitResolver};
use bolt_ts_scanner::is_line_break;
use bolt_ts_span::ModuleID;
use rustc_hash::FxHashSet;

use super::emit::print::PPrint;

macro_rules! dts {
    ($this:ident, $($body:tt)*) => {
        if $this.target == EmitTarget::Both {
            if let Some(dts) = &mut $this.dts {
                dts.$($body)*
            }
        }
    };
}

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
    let declaration = checker
        .config
        .compiler_options()
        .flags()
        .contains(bolt_ts_config::CompilerOptionFlags::DECLARATION);
    entries
        .into_iter()
        .filter_map(|item| {
            let is_default_lib = checker.module_arena.get_module(item).is_default_lib();
            if is_default_lib {
                None
            } else {
                let (js, dts) = emit_module(item, checker, declaration);
                Some((item, js, dts))
            }
        })
        .collect::<Vec<_>>()
}

fn emit_module<'cx>(
    module_id: ModuleID,
    checker: &mut TyChecker<'cx>,
    declaration: bool,
) -> (String, String) {
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
        dts: if declaration {
            Some(Emitter::new())
        } else {
            None
        },
        resolver,
        ns_names: FxHashSet::default(),
        scope: ScopeID::root(),
        max_scope: ScopeID::root().next(),
        origin,
        module_id,
        comment_idx: 0,
        flags,
        target: if declaration {
            EmitTarget::Both
        } else {
            EmitTarget::Js
        },
    };
    let root = emitter.resolver.program(module_id);
    emitter.visit_program(root);
    let js = emitter.js.print().take_content();
    let dts = match emitter.dts {
        Some(mut emitter) => emitter.print().take_content(),
        None => String::new(),
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum EmitTarget {
    Js,
    Declaration,
    Both,
}

struct ModuleEmitter<'cx, 'a> {
    js: Emitter,
    dts: Option<Emitter>,
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
        let saved = std::mem::replace(&mut self.target, target);
        f(self);
        self.target = saved;
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
            |this, decl| this.visit_var_decl(decl),
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
            |this, item| this.visit_param_decl(item),
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
            |this, arg| this.visit_expr(arg),
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

    fn emit_class_extends_clause(&mut self, extends: &'cx ast::ClassExtendsClause<'cx>) {
        self.emit_token(TokenKind::Extends);
        self.js.print().p_whitespace();
        self.visit_expr_with_ty_args(extends.expr_with_ty_args);
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
            self.visit_ident(decl_name);
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
        self.visit_ident(decl_name);
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
            self.visit_stmt(stmt);
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
                            self.visit_binding(item.name);
                            self.js.content.p_whitespace();
                            self.emit_token(TokenKind::Eq);
                            self.js.content.p_whitespace();
                            self.visit_binding(item.name);
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
                self.visit_ident(name);
                self.js.content.p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.content.p_whitespace();
                self.visit_ident(name);
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
                if self.target == EmitTarget::Both {
                    self.with_target(EmitTarget::Declaration, |this| this.visit_stmt(stmt));
                }
                continue;
            }
            if !first {
                self.js.content.p_newline();
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
            |this, item| this.visit_param_decl(item),
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
        if dts_parens {
            dts!(self, print().p_token(TokenKind::LParen));
        }
        let mut params = params;
        let mut dts_first = true;
        if let Some((first, rest)) = params.split_first()
            && self.is_this_param(first)
        {
            let pos = rest.first().map_or(first.span.hi(), |p| p.span.lo());
            self.sync_comment_idx(pos);
            self.with_target(EmitTarget::Declaration, |this| this.visit_param_decl(first));
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
                dts!(self, print().p_token(TokenKind::Comma));
                dts!(self, print().p_whitespace());
            }
            self.visit_param_decl(param);
            js_first = false;
            dts_first = false;
        }
        self.emit_token(TokenKind::RParen);
        if dts_parens {
            dts!(self, print().p_token(TokenKind::RParen));
        }
    }

    fn emit_class_body(&mut self, elems: &'cx ast::ClassElems<'cx>, dts_braces: bool) {
        let has_items = elems.list.iter().any(|e| !self.class_elem_is_empty(e));
        self.emit_token(TokenKind::LBrace);
        let dts_on = self.target == EmitTarget::Both;
        if dts_on && dts_braces {
            dts!(self, print().p_token(TokenKind::LBrace));
        }
        if has_items {
            self.js.print().indent += self.js.options.indent;
            self.js.print().p_newline();
        }
        let dts_wrap = dts_on && dts_braces && !elems.list.is_empty();
        if dts_wrap {
            dts!(self, increment_indent());
            dts!(self, print().p_newline());
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
                if dts_braces && !dts_first {
                    dts!(self, print().p_newline());
                }
                dts_first = false;
            }
            self.visit_class_elem(elem);
        }
        if has_items {
            self.js.print().indent -= self.js.options.indent;
            self.js.print().p_newline();
        }
        if dts_wrap {
            dts!(self, decrement_indent());
            dts!(self, print().p_newline());
        }
        self.emit_token(TokenKind::RBrace);
        if dts_on && dts_braces {
            dts!(self, print().p_token(TokenKind::RBrace));
        }
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
                self.emit_token(TokenKind::Var);
                self.js.print().p_whitespace();
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
                self.emit_token(TokenKind::DotDotDot);
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
            This(_) => self.emit_token(TokenKind::This),
        }
    }

    fn visit_jsx_attr(&mut self, node: &'cx ast::JsxAttr<'cx>) {
        match node {
            ast::JsxAttr::Spread(n) => {
                self.emit_token(TokenKind::LBrace);
                self.emit_token(TokenKind::DotDotDot);
                self.visit_expr(n.expr);
                self.emit_token(TokenKind::RBrace);
            }
            ast::JsxAttr::Named(n) => {
                self.visit_jsx_attr_name(n.name.clone());
                if let Some(v) = n.init {
                    self.emit_token(TokenKind::Eq);
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
                self.js.print().p(content);
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
                self.visit_ty(ty);
            }
            None => self.dts.as_mut().unwrap().print().p("any"),
        }
    }

    fn emit_type_parameters(&mut self, n: Option<ast::TyParams<'cx>>) {
        if let Some(n) = n {
            debug_assert!(!n.is_empty());
            self.dts.as_mut().unwrap().print().p_token(TokenKind::Less);
            self.emit_list_indexed(
                n,
                |this, item, _| {
                    this.visit_ident(item.name);
                    if let Some(constraint) = item.constraint {
                        this.dts.as_mut().unwrap().print().p_whitespace();
                        this.dts
                            .as_mut()
                            .unwrap()
                            .print()
                            .p_token(TokenKind::Extends);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                        this.visit_ty(constraint);
                    }
                    if let Some(default) = item.default {
                        this.dts.as_mut().unwrap().print().p_whitespace();
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Eq);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                        this.visit_ty(default);
                    }
                },
                |this, _, _| {
                    this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                    this.dts.as_mut().unwrap().print().p_whitespace();
                },
            );
            self.dts.as_mut().unwrap().print().p_token(TokenKind::Great);
        }
    }

    fn emit_type_arguments(&mut self, n: Option<&'cx ast::Tys<'cx>>) {
        if let Some(n) = n {
            debug_assert!(!n.list.is_empty());
            self.dts.as_mut().unwrap().print().p_token(TokenKind::Less);
            self.emit_list_indexed(
                n.list,
                |this, item, _| {
                    this.visit_ty(item);
                },
                |this, _, _| {
                    this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                    this.dts.as_mut().unwrap().print().p_whitespace();
                },
            );
            self.dts.as_mut().unwrap().print().p_token(TokenKind::Great);
        }
    }

    fn emit_string_literal(&mut self, atom: bolt_ts_atom::Atom) {
        let atom = self.resolver.atoms().get(atom);
        self.dts.as_mut().unwrap().print().p_string_literal(atom);
    }

    fn emit_default_modifier(&mut self, ms: Option<&'cx ast::Modifiers<'cx>>) {
        if let Some(ms) = ms
            && ms.flags.contains(ast::ModifierFlags::DEFAULT)
        {
            self.dts
                .as_mut()
                .unwrap()
                .print()
                .p_token(TokenKind::Default);
            self.dts.as_mut().unwrap().print().p_whitespace();
        }
    }

    fn emit_export_modifier_if_needed(&mut self, contain_export_modifier: bool) {
        if contain_export_modifier
            && !self
                .flags
                .contains(EmitDeclarationFlags::STRIP_EXPORT_MODIFIER)
        {
            self.dts
                .as_mut()
                .unwrap()
                .print()
                .p_token(TokenKind::Export);
            self.dts.as_mut().unwrap().print().p_whitespace();
        }
    }

    fn emit_declare_if_needed(&mut self) {
        if self.flags.contains(EmitDeclarationFlags::NEED_DECLARE) {
            self.dts
                .as_mut()
                .unwrap()
                .print()
                .p_token(TokenKind::Declare);
            self.dts.as_mut().unwrap().print().p_whitespace();
        }
    }

    fn emit_identifier_name_in_variable_declaration(
        &mut self,
        node: &'cx ast::Ident,
        binding: ast::NodeID,
    ) {
        self.dts
            .as_mut()
            .unwrap()
            .emit_atom(self.resolver.atoms(), node.name);
        self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
        self.dts.as_mut().unwrap().print().p_whitespace();
        let ty = self.resolver.ensure_type_for_identifier_in_binding(binding);
        let ty_str = self.resolver.print_type(ty);
        self.dts.as_mut().unwrap().print().p(&ty_str);
    }

    fn emit_variable_declaration_by_array_pat(&mut self, node: &'cx ast::ArrayPat<'cx>) {
        let elements = node.elems.iter().filter(|element| match element.kind {
            ast::ArrayBindingElemKind::Omit(_) => false,
            ast::ArrayBindingElemKind::Binding(_) => true,
        });

        for (idx, element) in elements.enumerate() {
            if idx != 0 {
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                self.dts.as_mut().unwrap().print().p_whitespace();
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
                this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                this.dts.as_mut().unwrap().print().p_whitespace();
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
                this.visit_param_decl(item);
            },
            |this, _, _| {
                this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                this.dts.as_mut().unwrap().print().p_whitespace();
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
}
impl<'cx, 'a> Visitor<'cx> for ModuleEmitter<'cx, 'a> {
    type Result = ();
    fn visit_program(&mut self, node: &'cx ast::Program<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                let mut first = true;
                for stmt in node.stmts() {
                    if self.stmt_is_omitted(stmt) {
                        self.sync_comment_idx(stmt.span().hi());
                        continue;
                    }
                    if !first {
                        self.js.content.p_newline();
                    }
                    self.visit_stmt(stmt);
                    first = false;
                }
            }
            EmitTarget::Declaration => {
                for stmt in node.stmts() {
                    self.visit_stmt(stmt);
                    self.dts.as_mut().unwrap().print().p_newline();
                }
            }
            EmitTarget::Both => {
                let mut first = true;
                for stmt in node.stmts() {
                    if self.stmt_is_omitted(stmt) {
                        self.sync_comment_idx(stmt.span().hi());
                        self.visit_stmt(stmt);
                        dts!(self, print().p_newline());
                        continue;
                    }
                    if !first {
                        self.js.content.p_newline();
                    }
                    self.visit_stmt(stmt);
                    dts!(self, print().p_newline());
                    first = false;
                }
            }
        }
    }
    fn visit_var_stmt(&mut self, node: &'cx ast::VarStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
            }
            EmitTarget::Declaration => {
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
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Const);
                } else if node_flags.contains(ast::NodeFlags::LET) {
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Let);
                } else {
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Var);
                }
                self.dts.as_mut().unwrap().print().p_whitespace();

                self.emit_list_indexed(
                    node.list,
                    |this, item, _| {
                        this.visit_var_decl(item);
                    },
                    |this, _, _| {
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                    },
                );
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                let js_on = !node
                    .modifiers
                    .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT));
                let dts_on = self.dts.is_some()
                    && node
                        .list
                        .iter()
                        .all(|n| self.has_name_visible_for_binding(n.name));
                match (js_on, dts_on) {
                    (true, true) => {
                        self.emit_token(TokenKind::Var);
                        self.js.print().p_whitespace();
                        self.emit_export_modifier_if_needed(contain_export_modifier(
                            node.modifiers,
                        ));
                        self.emit_declare_if_needed();
                        let node_flags = self.resolver.node_flags(node.id);
                        if node_flags.contains(ast::NodeFlags::CONST) {
                            dts!(self, print().p_token(TokenKind::Const));
                        } else if node_flags.contains(ast::NodeFlags::LET) {
                            dts!(self, print().p_token(TokenKind::Let));
                        } else {
                            dts!(self, print().p_token(TokenKind::Var));
                        }
                        dts!(self, print().p_whitespace());
                        for (idx, item) in node.list.iter().enumerate() {
                            if idx != 0 {
                                self.emit_token(TokenKind::Comma);
                                self.js.content.p_whitespace();
                                dts!(self, print().p_token(TokenKind::Comma));
                                dts!(self, print().p_whitespace());
                            }
                            self.visit_var_decl(item);
                        }
                        if next_adjacent_token_on_this_line_is_semi(
                            self.origin.as_bytes(),
                            node.span.hi() as usize - 1,
                        ) {
                            self.emit_token(TokenKind::Semi);
                        } else {
                            self.js.print().p_token(TokenKind::Semi);
                        }
                        dts!(self, print().p_token(TokenKind::Semi));
                    }
                    (true, false) => {
                        self.with_target(EmitTarget::Js, |this| this.visit_var_stmt(node))
                    }
                    (false, true) => {
                        self.with_target(EmitTarget::Declaration, |this| this.visit_var_stmt(node))
                    }
                    (false, false) => {}
                }
            }
        }
    }
    fn visit_var_decl(&mut self, node: &'cx ast::VarDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.visit_binding(node.name);
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
                    self.visit_expr(init);
                }
            }
            EmitTarget::Declaration => match node.name.kind {
                ast::BindingKind::Ident(name) => {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .emit_atom(self.resolver.atoms(), name.name);
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    if let Some(ty) = node.ty {
                        self.visit_ty(ty);
                    } else {
                        let ty = self.resolver.ensure_type_for_variable_declaration(node);
                        let ty_str = self.resolver.print_type(ty);
                        self.dts.as_mut().unwrap().print().p(&ty_str);
                    }
                }
                ast::BindingKind::ArrayPat(n) => self.emit_variable_declaration_by_array_pat(n),
                ast::BindingKind::ObjectPat(n) => self.emit_variable_declaration_by_object_pat(n),
            },
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_var_decl(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_var_decl(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_ident(&mut self, node: &'cx ast::Ident) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                let content = self.atoms().get(node.name);
                self.emit_token_value(TokenKind::Ident, content);
            }
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .emit_atom(self.resolver.atoms(), node.name);
            }
            EmitTarget::Both => {
                let content = self.atoms().get(node.name);
                self.emit_token_value(TokenKind::Ident, content);
                dts!(self, emit_atom(self.resolver.atoms(), node.name));
            }
        }
    }
    fn visit_private_ident(&mut self, node: &'cx ast::PrivateIdent) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_private_ident(self, node),
            EmitTarget::Js => {
                let content = self.atoms().get(node.name);
                self.emit_token_value(TokenKind::PrivateIdent, content);
            }
            EmitTarget::Both => {
                let content = self.atoms().get(node.name);
                self.emit_token_value(TokenKind::PrivateIdent, content);
            }
        }
    }
    fn visit_num_lit(&mut self, node: &'cx ast::NumLit) -> Self::Result {
        match self.target {
            EmitTarget::Js => self.emit_token_value(TokenKind::Number, &node.val.to_string()),
            EmitTarget::Declaration => {
                self.dts.as_mut().unwrap().print().p(&node.val.to_string());
            }
            EmitTarget::Both => {
                self.emit_token_value(TokenKind::Number, &node.val.to_string());
                dts!(self, print().p(&node.val.to_string()));
            }
        }
    }
    fn visit_string_lit(&mut self, node: &'cx ast::StringLit) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.emit_as_string(node.val);
            }
            EmitTarget::Declaration => {
                self.emit_string_literal(node.val);
            }
            EmitTarget::Both => {
                self.emit_as_string(node.val);
                if self.dts.is_some() {
                    self.emit_string_literal(node.val);
                }
            }
        }
    }
    fn visit_prop_name(&mut self, node: &'cx ast::PropName<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                use ast::PropNameKind::*;
                match &node.kind {
                    Ident(n) => self.visit_ident(n),
                    NumLit(n) => self.visit_num_lit(n),
                    StringLit { raw, .. } => self.visit_string_lit(raw),
                    Computed(n) => {
                        self.emit_token(TokenKind::LBracket);
                        self.visit_expr(n.expr);
                        self.emit_token(TokenKind::RBracket);
                    }
                    PrivateIdent(n) => self.visit_private_ident(n),
                    BigIntLit(n) => {
                        let content = self.atoms().get(n.val.1);
                        self.emit_token_value(TokenKind::BigInt, content);
                    }
                }
            }
            EmitTarget::Declaration => {
                use ast::PropNameKind::*;
                match &node.kind {
                    Ident(n) => self.visit_ident(n),
                    StringLit { raw, .. } => self.visit_string_lit(raw),
                    BigIntLit { .. } => todo!(),
                    NumLit(n) => self.visit_num_lit(n),
                    Computed(n) => {
                        self.visit_computed_prop_name(n);
                    }
                    PrivateIdent(_private_ident) => todo!(),
                }
            }
            EmitTarget::Both => {
                use ast::PropNameKind::*;
                match &node.kind {
                    Ident(n) => self.visit_ident(n),
                    NumLit(n) => self.visit_num_lit(n),
                    StringLit { raw, .. } => self.visit_string_lit(raw),
                    Computed(n) => {
                        self.emit_token(TokenKind::LBracket);
                        dts!(self, print().p_token(TokenKind::LBracket));
                        self.visit_expr(n.expr);
                        self.emit_token(TokenKind::RBracket);
                        dts!(self, print().p_token(TokenKind::RBracket));
                    }
                    PrivateIdent(n) => {
                        self.visit_private_ident(n);
                        if self.dts.is_some() {
                            todo!()
                        }
                    }
                    BigIntLit(n) => {
                        let content = self.atoms().get(n.val.1);
                        self.emit_token_value(TokenKind::BigInt, content);
                        if self.dts.is_some() {
                            todo!()
                        }
                    }
                }
            }
        }
    }
    fn visit_object_binding_elem(
        &mut self,
        node: &'cx ast::ObjectBindingElem<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                if node.dotdotdot.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                }
                match node.name {
                    ast::ObjectBindingName::Shorthand(ident) => {
                        self.visit_ident(ident);
                    }
                    ast::ObjectBindingName::Prop { prop_name, name } => {
                        self.visit_prop_name(prop_name);
                        self.emit_token(TokenKind::Colon);
                        self.js.print().p_whitespace();
                        self.visit_binding(name);
                    }
                }
                if let Some(init) = node.init {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Eq);
                    self.js.print().p_whitespace();
                    self.visit_expr(init);
                }
            }
            EmitTarget::Declaration => {
                if node.dotdotdot.is_some() {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::DotDotDot);
                }
                match node.name {
                    ast::ObjectBindingName::Shorthand(ident) => self.visit_ident(ident),
                    ast::ObjectBindingName::Prop { prop_name, name } => {
                        self.visit_prop_name(prop_name);
                        self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                        self.dts.as_mut().unwrap().print().p_whitespace();
                        self.visit_binding(name);
                    }
                }
            }
            EmitTarget::Both => {
                if node.dotdotdot.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                    dts!(self, print().p_token(TokenKind::DotDotDot));
                }
                match node.name {
                    ast::ObjectBindingName::Shorthand(ident) => {
                        self.visit_ident(ident);
                    }
                    ast::ObjectBindingName::Prop { prop_name, name } => {
                        self.visit_prop_name(prop_name);
                        self.emit_token(TokenKind::Colon);
                        dts!(self, print().p_token(TokenKind::Colon));
                        self.js.print().p_whitespace();
                        dts!(self, print().p_whitespace());
                        self.visit_binding(name);
                    }
                }
                if let Some(init) = node.init {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Eq);
                    self.js.print().p_whitespace();
                    self.with_target(EmitTarget::Js, |this| this.visit_expr(init));
                }
            }
        }
    }
    fn visit_array_binding(&mut self, node: &'cx ast::ArrayBinding<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                if node.dotdotdot.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                }
                self.visit_binding(node.name);
                if let Some(init) = node.init {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Eq);
                    self.js.print().p_whitespace();
                    self.visit_expr(init);
                }
            }
            EmitTarget::Declaration => {
                if node.dotdotdot.is_some() {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::DotDotDot);
                }
                self.visit_binding(node.name);
            }
            EmitTarget::Both => {
                if node.dotdotdot.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                    dts!(self, print().p_token(TokenKind::DotDotDot));
                }
                self.visit_binding(node.name);
                if let Some(init) = node.init {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Eq);
                    self.js.print().p_whitespace();
                    self.with_target(EmitTarget::Js, |this| this.visit_expr(init));
                }
            }
        }
    }
    fn visit_binding(&mut self, node: &'cx ast::Binding<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_binding(self, node),
            EmitTarget::Js => {
                use ast::BindingKind::*;
                match node.kind {
                    Ident(n) => self.visit_ident(n),
                    ObjectPat(n) => {
                        self.emit_token(TokenKind::LBrace);
                        self.emit_list(
                            n.elems,
                            |this, item| this.visit_object_binding_elem(item),
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
                                    this.visit_array_binding(n);
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
            }
            EmitTarget::Both => {
                use ast::BindingKind::*;
                match node.kind {
                    Ident(n) => self.visit_ident(n),
                    ObjectPat(n) => {
                        self.emit_token(TokenKind::LBrace);
                        dts!(self, print().p_token(TokenKind::LBrace));
                        for (idx, item) in n.elems.iter().enumerate() {
                            if idx != 0 {
                                self.emit_token(TokenKind::Comma);
                                self.js.content.p_whitespace();
                                dts!(self, print().p_token(TokenKind::Comma));
                                dts!(self, print().p_whitespace());
                            }
                            self.visit_object_binding_elem(item);
                        }
                        self.emit_token(TokenKind::RBrace);
                        dts!(self, print().p_token(TokenKind::RBrace));
                    }
                    ArrayPat(n) => {
                        self.emit_token(TokenKind::LBracket);
                        dts!(self, print().p_token(TokenKind::LBracket));
                        for (idx, item) in n.elems.iter().enumerate() {
                            if idx != 0 {
                                self.emit_token(TokenKind::Comma);
                                self.js.content.p_whitespace();
                                dts!(self, print().p_token(TokenKind::Comma));
                                dts!(self, print().p_whitespace());
                            }
                            match item.kind {
                                ast::ArrayBindingElemKind::Omit(_) => {}
                                ast::ArrayBindingElemKind::Binding(n) => {
                                    self.visit_array_binding(n);
                                }
                            }
                        }
                        self.emit_token(TokenKind::RBracket);
                        dts!(self, print().p_token(TokenKind::RBracket));
                    }
                }
            }
        }
    }
    fn visit_param_decl(&mut self, node: &'cx ast::ParamDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                if node.dotdotdot.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                }
                if node.modifiers.is_some() {
                    self.sync_comment_idx(node.name.span.lo());
                }
                self.visit_binding(node.name);
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
                    self.visit_expr(init);
                }
            }
            EmitTarget::Declaration => {
                let n = node;
                if n.dotdotdot.is_some() {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::DotDotDot);
                }
                self.visit_binding(n.name);
                if self.resolver.is_optional_parameter(n) {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Question);
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();

                if let Some(ty) = n.ty {
                    self.visit_ty(ty);
                } else {
                    let ty = self
                        .resolver
                        .ensure_type_for_parameter_declaration::<true>(n);
                    let ty = self.resolver.print_type(ty);
                    self.dts.as_mut().unwrap().print().p(&ty);
                }
            }
            EmitTarget::Both => {
                if node.dotdotdot.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                    dts!(self, print().p_token(TokenKind::DotDotDot));
                }
                if node.modifiers.is_some() {
                    self.sync_comment_idx(node.name.span.lo());
                }
                self.visit_binding(node.name);
                if let Some(question) = node.question {
                    self.sync_comment_idx(question.hi());
                }
                if self.dts.is_some() && self.resolver.is_optional_parameter(node) {
                    dts!(self, print().p_token(TokenKind::Question));
                }
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                if self.dts.is_some() {
                    dts!(self, print().p_token(TokenKind::Colon));
                    dts!(self, print().p_whitespace());
                    if let Some(ty) = node.ty {
                        self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                    } else {
                        let ty = self
                            .resolver
                            .ensure_type_for_parameter_declaration::<true>(node);
                        let ty = self.resolver.print_type(ty);
                        dts!(self, print().p(&ty));
                    }
                }
                if let Some(init) = node.init {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Eq);
                    self.js.print().p_whitespace();
                    self.with_target(EmitTarget::Js, |this| this.visit_expr(init));
                }
            }
        }
    }
    fn visit_block_stmt(&mut self, node: &'cx ast::BlockStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_block_stmt(self, node),
            EmitTarget::Js => {
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
            EmitTarget::Both => {
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
    }
    fn visit_if_stmt(&mut self, node: &'cx ast::IfStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_if_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::If);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.visit_stmt(node.then);
                if let Some(else_then) = node.else_then {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Else);
                    self.js.print().p_whitespace();
                    self.visit_stmt(else_then);
                }
                self.js.print().p_newline();
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::If);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.visit_stmt(node.then);
                if let Some(else_then) = node.else_then {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Else);
                    self.js.print().p_whitespace();
                    self.visit_stmt(else_then);
                }
                self.js.print().p_newline();
            }
        }
    }
    fn visit_ret_stmt(&mut self, node: &'cx ast::RetStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_ret_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Return);
                self.js.print().p_whitespace();
                if let Some(expr) = node.expr {
                    self.visit_expr(expr);
                }
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Return);
                self.js.print().p_whitespace();
                if let Some(expr) = node.expr {
                    self.visit_expr(expr);
                }
            }
        }
    }
    fn visit_getter_decl(&mut self, node: &'cx ast::GetterDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_getter_decl(self, node),
            EmitTarget::Js => {
                let Some(body) = node.body else {
                    self.sync_comment_idx(node.span.hi());
                    return;
                };
                self.emit_static_modifier(node.modifiers);
                self.emit_token(TokenKind::Get);
                self.js.print().p_whitespace();
                self.visit_prop_name(node.name);
                self.emit_params(&[]);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(body);
            }
            EmitTarget::Both => {
                let Some(body) = node.body else {
                    self.sync_comment_idx(node.span.hi());
                    self.with_target(EmitTarget::Declaration, |this| {
                        this.visit_prop_name(node.name);
                        if let Some(ty) = node.ty {
                            this.visit_ty(ty);
                        }
                    });
                    return;
                };
                self.emit_static_modifier(node.modifiers);
                self.emit_token(TokenKind::Get);
                self.js.print().p_whitespace();
                self.visit_prop_name(node.name);
                self.emit_params(&[]);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(body);
            }
        }
    }
    fn visit_setter_decl(&mut self, node: &'cx ast::SetterDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                let Some(body) = node.body else {
                    self.sync_comment_idx(node.span.hi());
                    return;
                };
                self.emit_static_modifier(node.modifiers);
                self.emit_token(TokenKind::Set);
                self.js.print().p_whitespace();
                self.visit_prop_name(node.name);
                self.emit_params_without_this(node.params);
                self.js.print().p_whitespace();
                self.visit_block_stmt(body);
            }
            EmitTarget::Declaration => {
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Set);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_prop_name(node.name);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
            }
            EmitTarget::Both => {
                let Some(body) = node.body else {
                    self.sync_comment_idx(node.span.hi());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_setter_decl(node));
                    return;
                };
                self.emit_static_modifier(node.modifiers);
                self.emit_token(TokenKind::Set);
                dts!(self, print().p_token(TokenKind::Set));
                self.js.print().p_whitespace();
                dts!(self, print().p_whitespace());
                self.visit_prop_name(node.name);
                self.emit_fn_params(node.params, true);
                self.js.print().p_whitespace();
                self.with_target(EmitTarget::Js, |this| this.visit_block_stmt(body));
            }
        }
    }
    fn visit_class_ctor(&mut self, node: &'cx ast::ClassCtor<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
                            this.visit_binding(param.name);
                            this.js.content.p_whitespace();
                            this.emit_token(TokenKind::Eq);
                            this.js.content.p_whitespace();
                            this.visit_binding(param.name);
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
            }
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Constructor);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
                if let Some(ty) = node.ret {
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_ty(ty);
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                let Some(body) = node.body else {
                    self.sync_comment_idx(node.span.hi());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_class_ctor(node));
                    return;
                };
                self.emit_token(TokenKind::Constructor);
                dts!(self, print().p_token(TokenKind::Constructor));
                self.emit_fn_params(node.params, true);
                if let Some(ty) = node.ret {
                    dts!(self, print().p_token(TokenKind::Colon));
                    dts!(self, print().p_whitespace());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                }
                dts!(self, print().p_token(TokenKind::Semi));
                self.js.print().p_whitespace();
                self.with_target(EmitTarget::Js, |this| {
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
                                this.visit_binding(param.name);
                                this.js.content.p_whitespace();
                                this.emit_token(TokenKind::Eq);
                                this.js.content.p_whitespace();
                                this.visit_binding(param.name);
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
    }
    fn visit_class_static_block_decl(
        &mut self,
        node: &'cx ast::ClassStaticBlockDecl<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => {
                bolt_ts_ast_visitor::visit_class_static_block_decl(self, node)
            }
            EmitTarget::Js => {
                self.emit_token(TokenKind::Static);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LBrace);
                self.visit_block_stmt(node.body);
                self.emit_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Static);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LBrace);
                self.visit_block_stmt(node.body);
                self.emit_token(TokenKind::RBrace);
            }
        }
    }
    fn visit_class_prop_elem(&mut self, node: &'cx ast::ClassPropElem<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
                self.visit_prop_name(node.name);
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
                    self.visit_expr(init);
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
            EmitTarget::Declaration => {
                if let Some(ms) = node.modifiers
                    && ms.flags.contains(ast::ModifierFlags::STATIC)
                {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Static);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                self.visit_prop_name(node.name);
                if node.question.is_some() {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Question);
                }
                if let Some(ty) = node.ty {
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_ty(ty);
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                if let Some(mods) = node.modifiers
                    && mods.flags.contains(ast::ModifierFlags::ABSTRACT)
                {
                    self.sync_comment_idx(node.span.hi());
                    self.with_target(EmitTarget::Declaration, |this| {
                        this.visit_class_prop_elem(node)
                    });
                    return;
                }
                self.emit_static_modifier(node.modifiers);
                if let Some(ms) = node.modifiers
                    && ms.flags.contains(ast::ModifierFlags::STATIC)
                {
                    dts!(self, print().p_token(TokenKind::Static));
                    dts!(self, print().p_whitespace());
                }
                if node.modifiers.is_some() {
                    self.sync_comment_idx(node.name.span().lo());
                }
                self.visit_prop_name(node.name);
                if let Some(question) = node.question {
                    self.sync_comment_idx(question.hi());
                }
                if node.question.is_some() {
                    dts!(self, print().p_token(TokenKind::Question));
                }
                if let Some(excl) = node.excl {
                    self.sync_comment_idx(excl.hi());
                }
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                if let Some(ty) = node.ty {
                    dts!(self, print().p_token(TokenKind::Colon));
                    dts!(self, print().p_whitespace());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                }
                dts!(self, print().p_token(TokenKind::Semi));
                if let Some(init) = node.init {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Eq);
                    self.js.print().p_whitespace();
                    self.with_target(EmitTarget::Js, |this| this.visit_expr(init));
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
    }
    fn visit_class_method_elem(&mut self, node: &'cx ast::ClassMethodElem<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                let Some(body) = node.body else {
                    self.sync_comment_idx(node.span.hi());
                    return;
                };
                self.emit_static_modifier(node.modifiers);
                if node.asterisk.is_some() {
                    self.emit_token(TokenKind::Asterisk);
                }
                self.visit_prop_name(node.name);
                self.emit_params_without_this(node.params);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(body);
            }
            EmitTarget::Declaration => {
                if let Some(ms) = node.modifiers
                    && ms.flags.contains(ast::ModifierFlags::STATIC)
                {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Static);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                self.visit_prop_name(node.name);
                self.emit_type_parameters(node.ty_params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                if let Some(ty) = node.ty {
                    self.visit_ty(ty);
                } else {
                    let ty = self.resolver.ensure_type_for_class_method_element(node);
                    let ty_str = self.resolver.print_type(ty);
                    self.dts.as_mut().unwrap().print().p(&ty_str);
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                let Some(body) = node.body else {
                    self.sync_comment_idx(node.span.hi());
                    self.with_target(EmitTarget::Declaration, |this| {
                        this.visit_class_method_elem(node)
                    });
                    return;
                };
                self.emit_static_modifier(node.modifiers);
                if let Some(ms) = node.modifiers
                    && ms.flags.contains(ast::ModifierFlags::STATIC)
                {
                    dts!(self, print().p_token(TokenKind::Static));
                    dts!(self, print().p_whitespace());
                }
                if node.asterisk.is_some() {
                    self.emit_token(TokenKind::Asterisk);
                }
                self.visit_prop_name(node.name);
                self.with_target(EmitTarget::Declaration, |this| {
                    this.emit_type_parameters(node.ty_params)
                });
                self.emit_fn_params(node.params, true);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                dts!(self, print().p_token(TokenKind::Colon));
                dts!(self, print().p_whitespace());
                if let Some(ty) = node.ty {
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                } else if self.dts.is_some() {
                    let ty = self.resolver.ensure_type_for_class_method_element(node);
                    let ty_str = self.resolver.print_type(ty);
                    dts!(self, print().p(&ty_str));
                }
                dts!(self, print().p_token(TokenKind::Semi));
                self.js.print().p_whitespace();
                self.with_target(EmitTarget::Js, |this| this.visit_block_stmt(body));
            }
        }
    }
    fn visit_class_elem(&mut self, node: &'cx ast::ClassElem<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_class_elem(self, node),
            EmitTarget::Js => {
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
            EmitTarget::Both => {
                use ast::ClassElemKind::*;
                match node.kind {
                    Prop(n) => self.visit_class_prop_elem(n),
                    Method(n) => self.visit_class_method_elem(n),
                    StaticBlockDecl(n) => self.visit_class_static_block_decl(n),
                    Ctor(n) => self.visit_class_ctor(n),
                    Getter(n) => self.visit_getter_decl(n),
                    Setter(n) => self.visit_setter_decl(n),
                    IndexSig(n) => {
                        self.with_target(EmitTarget::Declaration, |this| {
                            this.visit_index_sig_decl(n)
                        });
                    }
                    Semi(_) => {}
                }
            }
        }
    }
    fn visit_fn_decl(&mut self, node: &'cx ast::FnDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
                    self.visit_ident(name);
                }
                self.emit_params_without_this(node.params);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(body);
            }
            EmitTarget::Declaration => {
                let n = node;
                // TODO: top level
                if self
                    .resolver
                    .is_implementation_of_overload_for_function_declaration(n)
                {
                    return Self::Result::default();
                }
                self.emit_export_modifier_if_needed(contain_export_modifier(n.modifiers));
                self.emit_default_modifier(n.modifiers);
                self.emit_declare_if_needed();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Function);
                self.dts.as_mut().unwrap().print().p_whitespace();
                if let Some(name) = n.name.map(|name| name.name) {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .emit_atom(self.resolver.atoms(), name);
                }
                self.emit_type_parameters(n.ty_params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(n.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                if let Some(ty) = n.ty {
                    self.visit_ty(ty);
                } else {
                    let ty = self.resolver.ensure_type_for_function_declaration(n);
                    let ty_str = self.resolver.print_type(ty);
                    self.dts.as_mut().unwrap().print().p(&ty_str);
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                let js_on = node.body.is_some();
                let dts_on = self.dts.is_some()
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
                        self.emit_export_modifier_if_needed(contain_export_modifier(
                            node.modifiers,
                        ));
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
                        dts!(self, print().p_token(TokenKind::Function));
                        if node.asterisk.is_some() {
                            self.emit_token(TokenKind::Asterisk);
                        }
                        self.js.print().p_whitespace();
                        dts!(self, print().p_whitespace());
                        if let Some(name) = node.name {
                            self.visit_ident(name);
                        }
                        self.with_target(EmitTarget::Declaration, |this| {
                            this.emit_type_parameters(node.ty_params)
                        });
                        self.emit_fn_params(node.params, true);
                        if let Some(ty) = node.ty {
                            self.sync_comment_idx(ty.span().hi());
                        }
                        dts!(self, print().p_token(TokenKind::Colon));
                        dts!(self, print().p_whitespace());
                        if let Some(ty) = node.ty {
                            self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                        } else if self.dts.is_some() {
                            let ty = self.resolver.ensure_type_for_function_declaration(node);
                            let ty_str = self.resolver.print_type(ty);
                            dts!(self, print().p(&ty_str));
                        }
                        dts!(self, print().p_token(TokenKind::Semi));
                        self.js.print().p_whitespace();
                        self.with_target(EmitTarget::Js, |this| this.visit_block_stmt(body));
                    }
                    (true, false) => {
                        self.with_target(EmitTarget::Js, |this| this.visit_fn_decl(node))
                    }
                    (false, true) => {
                        self.with_target(EmitTarget::Declaration, |this| this.visit_fn_decl(node))
                    }
                    (false, false) => {}
                }
            }
        }
    }
    fn visit_class_decl(&mut self, node: &'cx ast::ClassDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
                    self.visit_ident(name);
                    self.ns_names.insert((self.scope, name.name));
                    self.js.print().p_whitespace();
                }
                if let Some(extends) = node.extends {
                    self.emit_class_extends_clause(extends);
                }
                self.emit_class_body(node.elems, true);
            }
            EmitTarget::Declaration => {
                self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
                self.emit_default_modifier(node.modifiers);
                self.emit_declare_if_needed();
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Class);
                self.dts.as_mut().unwrap().print().p_whitespace();
                if let Some(name) = node.name.map(|name| name.name) {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .emit_atom(self.resolver.atoms(), name);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                self.emit_type_parameters(node.ty_params);
                if let Some(extends) = node.extends {
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Extends);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_expr(extends.expr_with_ty_args.expr);
                    self.emit_type_arguments(extends.expr_with_ty_args.ty_args);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                if let Some(implements) = node.implements {
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Implements);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.emit_list_indexed(
                        implements.list,
                        |this, item, _| {
                            this.visit_refer_ty(item);
                        },
                        |this, _, _| {
                            this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                            this.dts.as_mut().unwrap().print().p_whitespace();
                        },
                    );

                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBrace);
                if !node.elems.list.is_empty() {
                    self.dts.as_mut().unwrap().increment_indent();
                    self.dts.as_mut().unwrap().print().p_newline();
                    self.emit_list_indexed(
                        node.elems.list,
                        |this, elem, _| {
                            bolt_ts_ast_visitor::visit_class_elem(this, elem);
                        },
                        |this, _, _| {
                            this.dts.as_mut().unwrap().print().p_newline();
                        },
                    );
                    self.dts.as_mut().unwrap().decrement_indent();
                    self.dts.as_mut().unwrap().print().p_newline();
                }
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                let js_on = !node
                    .modifiers
                    .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT));
                if !js_on {
                    self.with_target(EmitTarget::Declaration, |this| this.visit_class_decl(node));
                    return;
                }
                self.emit_export_modifier_if_root(node.modifiers);
                self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
                self.emit_default_modifier(node.modifiers);
                self.emit_declare_if_needed();
                self.emit_token(TokenKind::Class);
                dts!(self, print().p_token(TokenKind::Class));
                self.js.print().p_whitespace();
                dts!(self, print().p_whitespace());
                if let Some(name) = node.name {
                    self.visit_ident(name);
                    self.ns_names.insert((self.scope, name.name));
                    self.js.print().p_whitespace();
                    dts!(self, print().p_whitespace());
                }
                self.with_target(EmitTarget::Declaration, |this| {
                    this.emit_type_parameters(node.ty_params)
                });
                if let Some(extends) = node.extends {
                    self.emit_token(TokenKind::Extends);
                    self.js.print().p_whitespace();
                    dts!(self, print().p_whitespace());
                    dts!(self, print().p_token(TokenKind::Extends));
                    dts!(self, print().p_whitespace());
                    self.visit_expr(extends.expr_with_ty_args.expr);
                    self.sync_comment_idx(extends.expr_with_ty_args.span.hi());
                    if self.dts.is_some() {
                        self.with_target(EmitTarget::Declaration, |this| {
                            this.emit_type_arguments(extends.expr_with_ty_args.ty_args)
                        });
                    }
                    self.js.print().p_whitespace();
                    dts!(self, print().p_whitespace());
                }
                if let Some(implements) = node.implements {
                    dts!(self, print().p_whitespace());
                    dts!(self, print().p_token(TokenKind::Implements));
                    dts!(self, print().p_whitespace());
                    if self.dts.is_some() {
                        self.emit_list_indexed(
                            implements.list,
                            |this, item, _| {
                                this.visit_refer_ty(item);
                            },
                            |this, _, _| {
                                dts!(this, print().p_token(TokenKind::Comma));
                                dts!(this, print().p_whitespace());
                            },
                        );
                    }
                    dts!(self, print().p_whitespace());
                }
                self.emit_class_body(node.elems, true);
            }
        }
    }
    fn visit_class_expr(&mut self, node: &'cx ast::ClassExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_class_expr(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Class);
                self.js.print().p_whitespace();
                if let Some(name) = node.name {
                    self.visit_ident(name);
                    self.ns_names.insert((self.scope, name.name));
                    self.js.print().p_whitespace();
                }
                if let Some(extends) = node.extends {
                    self.emit_class_extends_clause(extends);
                }
                self.emit_class_body(node.elems, false);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Class);
                self.js.print().p_whitespace();
                if let Some(name) = node.name {
                    self.visit_ident(name);
                    self.ns_names.insert((self.scope, name.name));
                    self.js.print().p_whitespace();
                }
                if let Some(ty_params) = node.ty_params {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty_param in ty_params {
                            this.visit_ty_param(ty_param);
                        }
                    });
                }
                if let Some(extends) = node.extends {
                    self.emit_class_extends_clause(extends);
                }
                if let Some(implements) = node.implements {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for item in implements.list {
                            this.visit_refer_ty(item);
                        }
                    });
                }
                self.emit_class_body(node.elems, false);
            }
        }
    }
    fn visit_throw_stmt(&mut self, node: &'cx ast::ThrowStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_throw_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Throw);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Throw);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
            }
        }
    }
    fn visit_block_module_decl(&mut self, node: &'cx ast::BlockModuleDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.emit_block_module_decl(node);
            }
            EmitTarget::Declaration => {
                self.emit_declare_if_needed();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Namespace);
                self.dts.as_mut().unwrap().print().p_whitespace();
                visit_module_name(self, node.name);
                if let Some(block) = node.block {
                    self.visit_module_block(block)
                }
            }
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_block_module_decl(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_block_module_decl(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_nested_module_decl(&mut self, node: &'cx ast::NestedModuleDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
                            this.visit_nested_module_decl(inner);
                            this.js.content.p_newline();

                            this.js.content.p(&param_name);
                            this.emit_token(TokenKind::Dot);
                            this.visit_ident(inner.name);
                            this.js.content.p_whitespace();
                            this.emit_token(TokenKind::Eq);
                            this.js.content.p_whitespace();
                            this.visit_ident(inner.name);
                            this.emit_token(TokenKind::Semi);
                            this.js.content.p_newline();
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
            EmitTarget::Declaration => {
                let n = node;
                self.emit_declare_if_needed();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Namespace);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ident(n.name);
                match n.block {
                    ast::NestedModuleBlock::Nested(decl) => {
                        visit_nested_module_declaration_inner(self, decl);
                    }
                    ast::NestedModuleBlock::Block(block) => self.visit_module_block(block),
                }
            }
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_nested_module_decl(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_nested_module_decl(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_enum_decl(&mut self, node: &'cx ast::EnumDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                if node
                    .modifiers
                    .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
                {
                    return;
                }
                self.emit_with_var_fn_wrapper(
                    node.name,
                    self.atoms().get(node.name.name),
                    |this| {
                        for member in node.members {
                            this.js.content.p_newline();
                            this.visit_ident(node.name);
                            this.emit_token(TokenKind::LBracket);
                            this.visit_ident(node.name);
                            this.emit_token(TokenKind::LBracket);
                            this.emit_enum_member_name(&member.name);
                            this.emit_token(TokenKind::RBracket);
                            this.js.content.p_whitespace();
                            this.emit_token(TokenKind::Eq);
                            this.js.content.p_whitespace();
                            if let Some(init) = member.init {
                                this.visit_expr(init);
                            } else {
                                this.js.content.p("0");
                            }
                            this.emit_token(TokenKind::RBracket);
                            this.js.content.p_whitespace();
                            this.emit_token(TokenKind::Eq);
                            this.js.content.p_whitespace();
                            this.emit_enum_member_name(&member.name);
                        }
                    },
                );
            }
            EmitTarget::Declaration => {
                self.emit_declare_if_needed();
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Enum);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .emit_atom(self.resolver.atoms(), node.name.name);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBrace);
                self.dts.as_mut().unwrap().increment_indent();
                self.dts.as_mut().unwrap().print().p_newline();
                self.emit_list_indexed(
                    node.members,
                    |this, item, _| {
                        this.visit_enum_member(item);
                    },
                    |this, _, _| {
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                        this.dts.as_mut().unwrap().print().p_newline();
                    },
                );
                self.dts.as_mut().unwrap().decrement_indent();
                self.dts.as_mut().unwrap().print().p_newline();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_enum_decl(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_enum_decl(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_enum_member(&mut self, node: &'cx bolt_ts_ast::EnumMember<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                // Enum members are emitted inline by visit_enum_decl.
            }
            EmitTarget::Declaration => {
                bolt_ts_ast_visitor::visit_enum_member_name_kind(self, &node.name);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Eq);
                self.dts.as_mut().unwrap().print().p_whitespace();
                let enum_member_value = self.resolver.get_enum_member_value(node);
                match enum_member_value {
                    bolt_ts_checker::check::EnumMemberValue::Number(num) => {
                        self.dts.as_mut().unwrap().print().p(&num.to_string());
                    }
                    bolt_ts_checker::check::EnumMemberValue::Str(str) => {
                        self.dts
                            .as_mut()
                            .unwrap()
                            .emit_atom(self.resolver.atoms(), str);
                    }
                    bolt_ts_checker::check::EnumMemberValue::Err => {
                        self.dts.as_mut().unwrap().print().p("/* computed value */");
                    }
                };
            }
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_enum_member(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_enum_member(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_import_decl(&mut self, node: &'cx ast::ImportDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.emit_token(TokenKind::Import);
                self.js.print().p_whitespace();
                if let Some(clause) = node.clause {
                    self.visit_import_clause(clause);
                }
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::From);
                self.js.print().p_whitespace();
                self.visit_string_lit(node.module);
            }
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Import);
                self.dts.as_mut().unwrap().print().p_whitespace();
                if let Some(clause) = node.clause {
                    self.visit_import_clause(clause);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::From);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.emit_string_literal(node.module.val);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Import);
                self.js.print().p_whitespace();
                dts!(self, print().p_token(TokenKind::Import));
                dts!(self, print().p_whitespace());
                if let Some(clause) = node.clause {
                    self.visit_import_clause(clause);
                    dts!(self, print().p_whitespace());
                }
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::From);
                self.js.print().p_whitespace();
                dts!(self, print().p_token(TokenKind::From));
                dts!(self, print().p_whitespace());
                self.visit_string_lit(node.module);
            }
        }
    }
    fn visit_import_clause(&mut self, node: &'cx ast::ImportClause<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                if let Some(name) = node.name {
                    self.visit_ident(name);
                    self.js.print().p_whitespace();
                } else if let Some(kind) = node.kind {
                    match kind {
                        ast::ImportClauseKind::Specs(specs) => {
                            self.emit_list(
                                specs,
                                |this, spec| this.visit_import_spec(spec),
                                |this, _| {
                                    this.emit_token(TokenKind::Comma);
                                    this.js.content.p_whitespace();
                                },
                            );
                        }
                        ast::ImportClauseKind::Ns(ns) => self.visit_ns_import(ns),
                    }
                }
            }
            EmitTarget::Declaration => {
                if let Some(name) = node.name {
                    self.visit_ident(name);
                }
                if let Some(kind) = node.kind {
                    match kind {
                        ast::ImportClauseKind::Ns(n) => self.visit_ns_import(n),
                        ast::ImportClauseKind::Specs(list) => {
                            self.dts
                                .as_mut()
                                .unwrap()
                                .print()
                                .p_token(TokenKind::LBrace);
                            self.dts.as_mut().unwrap().print().p_whitespace();
                            self.emit_list_indexed(
                                list,
                                |this, item, _| match item.kind {
                                    ast::ImportSpecKind::Named(n) => {
                                        this.visit_import_named_spec(n)
                                    }
                                    ast::ImportSpecKind::Shorthand(n) => {
                                        this.visit_import_shorthand_spec(n)
                                    }
                                },
                                |this, _, _| {
                                    this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                                    this.dts.as_mut().unwrap().print().p_whitespace();
                                },
                            );
                            self.dts.as_mut().unwrap().print().p_whitespace();
                            self.dts
                                .as_mut()
                                .unwrap()
                                .print()
                                .p_token(TokenKind::RBrace);
                        }
                    }
                }
            }
            EmitTarget::Both => {
                if let Some(name) = node.name {
                    self.visit_ident(name);
                    self.js.print().p_whitespace();
                    if let Some(kind) = node.kind {
                        match kind {
                            ast::ImportClauseKind::Ns(n) => {
                                self.with_target(EmitTarget::Declaration, |this| {
                                    this.visit_ns_import(n)
                                });
                            }
                            ast::ImportClauseKind::Specs(list) => {
                                dts!(self, print().p_token(TokenKind::LBrace));
                                dts!(self, print().p_whitespace());
                                self.emit_list_indexed(
                                    list,
                                    |this, item, _| {
                                        this.with_target(
                                            EmitTarget::Declaration,
                                            |this| match item.kind {
                                                ast::ImportSpecKind::Named(n) => {
                                                    this.visit_import_named_spec(n)
                                                }
                                                ast::ImportSpecKind::Shorthand(n) => {
                                                    this.visit_import_shorthand_spec(n)
                                                }
                                            },
                                        );
                                    },
                                    |this, _, _| {
                                        dts!(this, print().p_token(TokenKind::Comma));
                                        dts!(this, print().p_whitespace());
                                    },
                                );
                                dts!(self, print().p_whitespace());
                                dts!(self, print().p_token(TokenKind::RBrace));
                            }
                        }
                    }
                } else if let Some(kind) = node.kind {
                    match kind {
                        ast::ImportClauseKind::Specs(specs) => {
                            dts!(self, print().p_token(TokenKind::LBrace));
                            dts!(self, print().p_whitespace());
                            for (idx, spec) in specs.iter().enumerate() {
                                if idx != 0 {
                                    self.emit_token(TokenKind::Comma);
                                    self.js.content.p_whitespace();
                                    dts!(self, print().p_token(TokenKind::Comma));
                                    dts!(self, print().p_whitespace());
                                }
                                self.visit_import_spec(spec);
                            }
                            dts!(self, print().p_whitespace());
                            dts!(self, print().p_token(TokenKind::RBrace));
                        }
                        ast::ImportClauseKind::Ns(ns) => self.visit_ns_import(ns),
                    }
                }
            }
        }
    }
    fn visit_ns_import(&mut self, node: &'cx ast::NsImport<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_ns_import(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Asterisk);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::As);
                self.js.print().p_whitespace();
                self.visit_ident(node.name);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Asterisk);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::As);
                self.js.print().p_whitespace();
                self.visit_ident(node.name);
            }
        }
    }
    fn visit_module_export_name(&mut self, node: &'cx ast::ModuleExportName<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_module_export_name(self, node),
            EmitTarget::Js => match node.kind {
                ast::ModuleExportNameKind::Ident(ident) => self.visit_ident(ident),
                ast::ModuleExportNameKind::StringLit(lit) => self.visit_string_lit(lit),
            },
            EmitTarget::Both => match node.kind {
                ast::ModuleExportNameKind::Ident(ident) => self.visit_ident(ident),
                ast::ModuleExportNameKind::StringLit(lit) => self.visit_string_lit(lit),
            },
        }
    }
    fn visit_import_shorthand_spec(
        &mut self,
        node: &'cx ast::ImportShorthandSpec<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.visit_ident(node.name);
            }
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .emit_atom(self.resolver.atoms(), node.name.name);
            }
            EmitTarget::Both => {
                self.visit_ident(node.name);
            }
        }
    }
    fn visit_import_named_spec(&mut self, node: &'cx ast::ImportNamedSpec<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.visit_module_export_name(node.prop_name);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::As);
                self.js.print().p_whitespace();
                self.visit_ident(node.name);
            }
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .emit_atom(self.resolver.atoms(), node.name.name);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts.as_mut().unwrap().print().p_token(TokenKind::As);
                self.dts.as_mut().unwrap().print().p_whitespace();
                match node.prop_name.kind {
                    ast::ModuleExportNameKind::Ident(n) => {
                        self.visit_ident(n);
                    }
                    ast::ModuleExportNameKind::StringLit(n) => {
                        self.visit_string_lit(n);
                    }
                }
            }
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_import_named_spec(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_import_named_spec(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_stmt(&mut self, node: &'cx ast::Stmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_stmt(self, node),
            EmitTarget::Js => {
                use ast::StmtKind::*;
                match node.kind {
                    Var(n) => self.visit_var_stmt(n),
                    Expr(n) => {
                        self.visit_expr_stmt(n);
                        if next_adjacent_token_on_this_line_is_semi(
                            self.origin.as_bytes(),
                            n.span.hi() as usize - 1,
                        ) {
                            self.emit_token(TokenKind::Semi);
                        } else {
                            self.js.print().p_token(TokenKind::Semi);
                        }
                    }
                    Fn(n) => self.visit_fn_decl(n),
                    If(n) => self.visit_if_stmt(n),
                    Block(n) => self.visit_block_stmt(n),
                    Ret(n) => {
                        self.visit_ret_stmt(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    Class(n) => self.visit_class_decl(n),
                    Throw(n) => {
                        self.visit_throw_stmt(n);
                        if next_adjacent_token_on_this_line_is_semi(
                            self.origin.as_bytes(),
                            n.span.hi() as usize - 1,
                        ) {
                            self.emit_token(TokenKind::Semi);
                        } else {
                            self.js.print().p_token(TokenKind::Semi);
                        }
                    }
                    NestedModule(n) => self.visit_nested_module_decl(n),
                    BlockModule(n) => self.visit_block_module_decl(n),
                    Enum(n) => self.visit_enum_decl(n),
                    Import(n) => self.visit_import_decl(n),
                    ImportEquals(n) => self.visit_import_equals_decl(n),
                    Export(n) => self.visit_export_decl(n),
                    ExportAssign(n) => {
                        self.visit_export_assign(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    For(n) => self.visit_for_stmt(n),
                    ForOf(n) => self.visit_for_of_stmt(n),
                    ForIn(n) => self.visit_for_in_stmt(n),
                    Break(n) => {
                        self.visit_break_stmt(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    Continue(n) => {
                        self.visit_continue_stmt(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    Try(n) => self.visit_try_stmt(n),
                    While(n) => self.visit_while_stmt(n),
                    Do(n) => self.visit_do_while_stmt(n),
                    Labeled(n) => self.visit_labeled_stmt(n),
                    Empty(_) => self.emit_token(TokenKind::Semi),
                    Switch(n) => self.visit_switch_stmt(n),
                    Interface(_) | TypeAlias(_) | Debugger(_) => {}
                }
            }
            EmitTarget::Both => {
                use ast::StmtKind::*;
                match node.kind {
                    Var(n) => self.visit_var_stmt(n),
                    Expr(n) => {
                        self.visit_expr_stmt(n);
                        if next_adjacent_token_on_this_line_is_semi(
                            self.origin.as_bytes(),
                            n.span.hi() as usize - 1,
                        ) {
                            self.emit_token(TokenKind::Semi);
                        } else {
                            self.js.print().p_token(TokenKind::Semi);
                        }
                    }
                    Fn(n) => self.visit_fn_decl(n),
                    If(n) => self.visit_if_stmt(n),
                    Block(n) => self.visit_block_stmt(n),
                    Ret(n) => {
                        self.visit_ret_stmt(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    Class(n) => self.visit_class_decl(n),
                    Throw(n) => {
                        self.visit_throw_stmt(n);
                        if next_adjacent_token_on_this_line_is_semi(
                            self.origin.as_bytes(),
                            n.span.hi() as usize - 1,
                        ) {
                            self.emit_token(TokenKind::Semi);
                        } else {
                            self.js.print().p_token(TokenKind::Semi);
                        }
                    }
                    NestedModule(n) => self.visit_nested_module_decl(n),
                    BlockModule(n) => self.visit_block_module_decl(n),
                    Enum(n) => self.visit_enum_decl(n),
                    Import(n) => self.visit_import_decl(n),
                    ImportEquals(n) => self.visit_import_equals_decl(n),
                    Export(n) => self.visit_export_decl(n),
                    ExportAssign(n) => {
                        self.visit_export_assign(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    For(n) => self.visit_for_stmt(n),
                    ForOf(n) => self.visit_for_of_stmt(n),
                    ForIn(n) => self.visit_for_in_stmt(n),
                    Break(n) => {
                        self.visit_break_stmt(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    Continue(n) => {
                        self.visit_continue_stmt(n);
                        self.emit_token(TokenKind::Semi);
                    }
                    Try(n) => self.visit_try_stmt(n),
                    While(n) => self.visit_while_stmt(n),
                    Do(n) => self.visit_do_while_stmt(n),
                    Labeled(n) => self.visit_labeled_stmt(n),
                    Empty(_) => self.emit_token(TokenKind::Semi),
                    Switch(n) => self.visit_switch_stmt(n),
                    Interface(n) => {
                        self.with_target(EmitTarget::Declaration, |this| {
                            this.visit_interface_decl(n)
                        });
                    }
                    TypeAlias(n) => {
                        self.with_target(EmitTarget::Declaration, |this| {
                            this.visit_type_alias_decl(n)
                        });
                    }
                    Debugger(_) => {}
                }
            }
        }
    }
    fn visit_switch_stmt(&mut self, node: &'cx ast::SwitchStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_switch_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Switch);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LBrace);
                self.visit_case_block(node.case_block);
                self.emit_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Switch);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LBrace);
                self.visit_case_block(node.case_block);
                self.emit_token(TokenKind::RBrace);
            }
        }
    }
    fn visit_case_block(&mut self, node: &'cx ast::CaseBlock<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_case_block(self, node),
            EmitTarget::Js => {
                if !node.clauses.is_empty() {
                    self.js.print().indent += self.js.options.indent;
                    self.js.print().p_newline();
                }
                self.emit_list(
                    node.clauses,
                    |this, item| match item {
                        ast::CaseOrDefaultClause::Case(n) => this.visit_case_clause(n),
                        ast::CaseOrDefaultClause::Default(n) => this.visit_default_clause(n),
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
            EmitTarget::Both => {
                if !node.clauses.is_empty() {
                    self.js.print().indent += self.js.options.indent;
                    self.js.print().p_newline();
                }
                self.emit_list(
                    node.clauses,
                    |this, item| match item {
                        ast::CaseOrDefaultClause::Case(n) => this.visit_case_clause(n),
                        ast::CaseOrDefaultClause::Default(n) => this.visit_default_clause(n),
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
    }
    fn visit_case_clause(&mut self, node: &'cx ast::CaseClause<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_case_clause(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Case);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
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
            EmitTarget::Both => {
                self.emit_token(TokenKind::Case);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
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
    }
    fn visit_default_clause(&mut self, node: &'cx ast::DefaultClause<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_default_clause(self, node),
            EmitTarget::Js => {
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
            EmitTarget::Both => {
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
    }
    fn visit_export_assign(&mut self, node: &'cx ast::ExportAssign<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.emit_token(TokenKind::Export);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Default);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
            }
            EmitTarget::Declaration => {
                if node.is_export_equals {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Export);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Eq);
                } else {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Export);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Default);
                }
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_expr(node.expr);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Export);
                self.js.print().p_whitespace();
                if node.is_export_equals {
                    dts!(self, print().p_token(TokenKind::Export));
                    dts!(self, print().p_whitespace());
                    dts!(self, print().p_token(TokenKind::Eq));
                } else {
                    dts!(self, print().p_token(TokenKind::Export));
                    dts!(self, print().p_whitespace());
                    dts!(self, print().p_token(TokenKind::Default));
                }
                self.emit_token(TokenKind::Default);
                self.js.print().p_whitespace();
                dts!(self, print().p_whitespace());
                self.visit_expr(node.expr);
                dts!(self, print().p_token(TokenKind::Semi));
            }
        }
    }
    fn visit_labeled_stmt(&mut self, node: &'cx ast::LabeledStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_labeled_stmt(self, node),
            EmitTarget::Js => {
                self.visit_ident(node.label);
                self.emit_token(TokenKind::Colon);
                self.js.print().p_whitespace();
                self.visit_stmt(node.stmt);
            }
            EmitTarget::Both => {
                self.visit_ident(node.label);
                self.emit_token(TokenKind::Colon);
                self.js.print().p_whitespace();
                self.visit_stmt(node.stmt);
            }
        }
    }
    fn visit_do_while_stmt(&mut self, node: &'cx ast::DoWhileStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_do_while_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Do);
                self.js.print().p_whitespace();
                self.visit_stmt(node.stmt);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::While);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Do);
                self.js.print().p_whitespace();
                self.visit_stmt(node.stmt);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::While);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
            }
        }
    }
    fn visit_while_stmt(&mut self, node: &'cx ast::WhileStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_while_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::While);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.visit_stmt(node.stmt);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::While);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.visit_stmt(node.stmt);
            }
        }
    }
    fn visit_catch_clause(&mut self, node: &'cx ast::CatchClause<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_catch_clause(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Catch);
                self.js.print().p_whitespace();
                if let Some(var) = node.var {
                    self.emit_token(TokenKind::LParen);
                    self.visit_var_decl(var);
                    self.emit_token(TokenKind::RParen);
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.block);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Catch);
                self.js.print().p_whitespace();
                if let Some(var) = node.var {
                    self.emit_token(TokenKind::LParen);
                    self.visit_var_decl(var);
                    self.emit_token(TokenKind::RParen);
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.block);
            }
        }
    }
    fn visit_try_stmt(&mut self, node: &'cx ast::TryStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_try_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Try);
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.try_block);
                if let Some(catch) = node.catch_clause {
                    self.js.print().p_whitespace();
                    self.visit_catch_clause(catch);
                }
                if let Some(finally) = node.finally_block {
                    self.emit_token(TokenKind::Finally);
                    self.js.print().p_whitespace();
                    self.visit_block_stmt(finally);
                }
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Try);
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.try_block);
                if let Some(catch) = node.catch_clause {
                    self.js.print().p_whitespace();
                    self.visit_catch_clause(catch);
                }
                if let Some(finally) = node.finally_block {
                    self.emit_token(TokenKind::Finally);
                    self.js.print().p_whitespace();
                    self.visit_block_stmt(finally);
                }
            }
        }
    }
    fn visit_continue_stmt(&mut self, node: &'cx ast::ContinueStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_continue_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Continue);
                if let Some(label) = node.label {
                    self.js.print().p_whitespace();
                    self.visit_ident(label);
                }
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Continue);
                if let Some(label) = node.label {
                    self.js.print().p_whitespace();
                    self.visit_ident(label);
                }
            }
        }
    }
    fn visit_break_stmt(&mut self, node: &'cx ast::BreakStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_break_stmt(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Break);
                if let Some(label) = node.label {
                    self.js.print().p_whitespace();
                    self.visit_ident(label);
                }
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Break);
                if let Some(label) = node.label {
                    self.js.print().p_whitespace();
                    self.visit_ident(label);
                }
            }
        }
    }
    fn visit_for_in_stmt(&mut self, node: &'cx ast::ForInStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.emit_token(TokenKind::For);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::LParen);
                self.js.print().p_whitespace();
                self.emit_for_init(node.init);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::In);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.visit_stmt(node.body);
            }
            EmitTarget::Declaration => {}
            EmitTarget::Both => {
                self.with_target(EmitTarget::Js, |this| this.visit_for_in_stmt(node));
            }
        }
    }
    fn visit_for_stmt(&mut self, node: &'cx ast::ForStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
                    self.visit_expr(cond);
                }
                self.emit_token(TokenKind::Semi);
                self.js.print().p_whitespace();
                if let Some(incr) = node.incr {
                    self.visit_expr(incr);
                }
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.visit_stmt(node.body);
            }
            EmitTarget::Declaration => {}
            EmitTarget::Both => {
                self.with_target(EmitTarget::Js, |this| this.visit_for_stmt(node));
            }
        }
    }
    fn visit_for_of_stmt(&mut self, node: &'cx ast::ForOfStmt<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
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
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
                self.js.print().p_whitespace();
                self.visit_stmt(node.body);
            }
            EmitTarget::Declaration => {}
            EmitTarget::Both => {
                self.with_target(EmitTarget::Js, |this| this.visit_for_of_stmt(node));
            }
        }
    }
    fn visit_export_decl(&mut self, node: &'cx ast::ExportDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_export_decl(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Export);
                self.js.print().p_whitespace();
                match node.clause.kind {
                    ast::ExportClauseKind::Specs(specs) => {
                        self.emit_token(TokenKind::LBrace);
                        self.js.print().p_whitespace();
                        self.emit_list(
                            specs.list,
                            |this, spec| this.visit_export_spec(spec),
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
                            self.visit_string_lit(module);
                        }
                    }
                    ast::ExportClauseKind::Ns(n) => self.visit_ns_export(n),
                    ast::ExportClauseKind::Glob(n) => {
                        self.emit_token(TokenKind::Asterisk);
                        self.js.print().p_whitespace();
                        self.emit_token(TokenKind::From);
                        self.js.print().p_whitespace();
                        self.visit_string_lit(n.module);
                    }
                }
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Export);
                self.js.print().p_whitespace();
                match node.clause.kind {
                    ast::ExportClauseKind::Specs(specs) => {
                        self.emit_token(TokenKind::LBrace);
                        self.js.print().p_whitespace();
                        self.emit_list(
                            specs.list,
                            |this, spec| this.visit_export_spec(spec),
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
                            self.visit_string_lit(module);
                        }
                    }
                    ast::ExportClauseKind::Ns(n) => self.visit_ns_export(n),
                    ast::ExportClauseKind::Glob(n) => {
                        self.emit_token(TokenKind::Asterisk);
                        self.js.print().p_whitespace();
                        self.emit_token(TokenKind::From);
                        self.js.print().p_whitespace();
                        self.visit_string_lit(n.module);
                    }
                }
            }
        }
    }
    fn visit_export_named_spec(&mut self, node: &'cx ast::ExportNamedSpec<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_export_named_spec(self, node),
            EmitTarget::Js => {
                self.visit_module_export_name(node.prop_name);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::As);
                self.js.print().p_whitespace();
                self.visit_module_export_name(node.name);
            }
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_export_named_spec(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_export_named_spec(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_export_shorthand_spec(
        &mut self,
        node: &'cx ast::ExportShorthandSpec<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_export_shorthand_spec(self, node),
            EmitTarget::Js => {
                self.visit_ident(node.name);
            }
            EmitTarget::Both => {
                self.visit_ident(node.name);
            }
        }
    }
    fn visit_ns_export(&mut self, node: &'cx ast::NsExport<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_ns_export(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Asterisk);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::As);
                self.js.print().p_whitespace();
                self.visit_module_export_name(node.name);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::From);
                self.js.print().p_whitespace();
                self.visit_string_lit(node.module);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Asterisk);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::As);
                self.js.print().p_whitespace();
                self.visit_module_export_name(node.name);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::From);
                self.js.print().p_whitespace();
                self.visit_string_lit(node.module);
            }
        }
    }
    fn visit_assign_expr(&mut self, node: &'cx ast::AssignExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.visit_expr(node.left);
                self.js.print().p_whitespace();
                let token = match node.op {
                    ast::AssignOp::LogicalAndEq | ast::AssignOp::LogicalOrEq => TokenKind::Unknown,
                    _ => node.op.into(),
                };
                self.emit_token_value(token, node.op.as_str());
                self.js.print().p_whitespace();
                self.visit_expr(node.right);
            }
            EmitTarget::Declaration => {}
            EmitTarget::Both => {
                self.with_target(EmitTarget::Js, |this| this.visit_assign_expr(node));
            }
        }
    }
    fn visit_bin_expr(&mut self, node: &'cx ast::BinExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_bin_expr(self, node),
            EmitTarget::Js => {
                self.visit_expr(node.left);
                self.js.print().p_whitespace();
                self.emit_token_value(node.op.into(), node.op.as_str());
                self.js.print().p_whitespace();
                self.visit_expr(node.right);
            }
            EmitTarget::Both => {
                self.visit_expr(node.left);
                self.js.print().p_whitespace();
                self.emit_token_value(node.op.into(), node.op.as_str());
                self.js.print().p_whitespace();
                self.visit_expr(node.right);
            }
        }
    }
    fn visit_paren_expr(&mut self, node: &'cx ast::ParenExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_paren_expr(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::LParen);
                self.visit_expr(node.expr);
                self.emit_token(TokenKind::RParen);
            }
        }
    }
    fn visit_this_expr(&mut self, _node: &'cx ast::ThisExpr) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_this_expr(self, _node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::This);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::This);
            }
        }
    }
    fn visit_big_int_lit(&mut self, node: &'cx ast::BigIntLit) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_big_int_lit(self, node),
            EmitTarget::Js => {
                if node.val.0 {
                    self.emit_token(TokenKind::Minus);
                }
                let content = self.atoms().get(node.val.1);
                self.emit_token_value(TokenKind::BigInt, &format!("{content}n"));
            }
            EmitTarget::Both => {
                if node.val.0 {
                    self.emit_token(TokenKind::Minus);
                }
                let content = self.atoms().get(node.val.1);
                self.emit_token_value(TokenKind::BigInt, &format!("{content}n"));
            }
        }
    }
    fn visit_reg_exp_lit(&mut self, node: &'cx ast::RegExpLit) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_reg_exp_lit(self, node),
            EmitTarget::Js => {
                let content = self.atoms().get(node.val);
                self.emit_token_value(TokenKind::Regexp, content);
            }
            EmitTarget::Both => {
                let content = self.atoms().get(node.val);
                self.emit_token_value(TokenKind::Regexp, content);
            }
        }
    }
    fn visit_array_lit(&mut self, node: &'cx ast::ArrayLit<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_array_lit(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::LBracket);
                for (idx, expr) in node.elems.iter().enumerate() {
                    self.visit_expr(expr);
                    if idx != node.elems.len() - 1 {
                        self.emit_token(TokenKind::Comma);
                        self.js.print().p_whitespace();
                    }
                }
                self.emit_token(TokenKind::RBracket);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::LBracket);
                for (idx, expr) in node.elems.iter().enumerate() {
                    self.visit_expr(expr);
                    if idx != node.elems.len() - 1 {
                        self.emit_token(TokenKind::Comma);
                        self.js.print().p_whitespace();
                    }
                }
                self.emit_token(TokenKind::RBracket);
            }
        }
    }
    fn visit_object_lit(&mut self, node: &'cx ast::ObjectLit<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_object_lit(self, node),
            EmitTarget::Js => {
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
                        this.visit_object_member(member);
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
            EmitTarget::Both => {
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
                        this.visit_object_member(member);
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
    }
    fn visit_object_method_member(
        &mut self,
        node: &'cx ast::ObjectMethodMember<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_object_method_member(self, node),
            EmitTarget::Js => {
                if node.asterisk.is_some() {
                    self.emit_token(TokenKind::Asterisk);
                }
                self.visit_prop_name(node.name);
                self.emit_params_without_this(node.params);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.body);
            }
            EmitTarget::Both => {
                if node.asterisk.is_some() {
                    self.emit_token(TokenKind::Asterisk);
                }
                self.visit_prop_name(node.name);
                if let Some(ty_params) = node.ty_params {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty_param in ty_params {
                            this.visit_ty_param(ty_param);
                        }
                    });
                }
                self.emit_fn_params(node.params, false);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.body);
            }
        }
    }
    fn visit_object_prop_assignment(
        &mut self,
        node: &'cx ast::ObjectPropAssignment<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => {
                bolt_ts_ast_visitor::visit_object_prop_assignment(self, node)
            }
            EmitTarget::Js => {
                self.visit_prop_name(node.name);
                self.emit_token(TokenKind::Colon);
                self.js.print().p_whitespace();
                self.visit_expr(node.init);
            }
            EmitTarget::Both => {
                self.visit_prop_name(node.name);
                self.emit_token(TokenKind::Colon);
                self.js.print().p_whitespace();
                self.visit_expr(node.init);
            }
        }
    }
    fn visit_object_shorthand_member(
        &mut self,
        node: &'cx ast::ObjectShorthandMember<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => {
                bolt_ts_ast_visitor::visit_object_shorthand_member(self, node)
            }
            EmitTarget::Js => {
                self.visit_ident(node.name);
            }
            EmitTarget::Both => {
                self.visit_ident(node.name);
                if let Some(init) = node.object_assignment_initializer {
                    self.with_target(EmitTarget::Declaration, |this| this.visit_expr(init));
                }
            }
        }
    }
    fn visit_prop_access_expr(&mut self, node: &'cx ast::PropAccessExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_prop_access_expr(self, node),
            EmitTarget::Js => {
                if let ast::ExprKind::NumLit(n) = node.expr.kind {
                    self.visit_num_lit(n);
                    if n.val.fract() == 0. {
                        self.emit_token(TokenKind::Dot);
                    }
                } else {
                    self.visit_expr(node.expr);
                }
                self.emit_token(TokenKind::Dot);
                self.visit_ident(node.name);
            }
            EmitTarget::Both => {
                if let ast::ExprKind::NumLit(n) = node.expr.kind {
                    self.visit_num_lit(n);
                    if n.val.fract() == 0. {
                        self.emit_token(TokenKind::Dot);
                    }
                } else {
                    self.visit_expr(node.expr);
                }
                self.emit_token(TokenKind::Dot);
                self.visit_ident(node.name);
            }
        }
    }
    fn visit_template_expr(&mut self, node: &'cx ast::TemplateExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_template_expr(self, node),
            EmitTarget::Js => {
                // visit_template_head
                self.emit_token(TokenKind::Backtick);
                let content = self.atoms().get(node.head.text);
                let content = escape_snippet_text(content);
                self.js.print().p(&content);
                for span in node.spans {
                    self.js.print().p_dollar_and_brace();
                    self.visit_expr(span.expr);
                    self.emit_token(TokenKind::RBrace);
                    let content = self.atoms().get(span.text);
                    self.js.print().p(content);
                }
                self.js.print().p_token(TokenKind::Backtick);
            }
            EmitTarget::Both => {
                // visit_template_head
                self.emit_token(TokenKind::Backtick);
                let content = self.atoms().get(node.head.text);
                let content = escape_snippet_text(content);
                self.js.print().p(&content);
                for span in node.spans {
                    self.js.print().p_dollar_and_brace();
                    self.visit_expr(span.expr);
                    self.emit_token(TokenKind::RBrace);
                    let content = self.atoms().get(span.text);
                    self.js.print().p(content);
                }
                self.js.print().p_token(TokenKind::Backtick);
            }
        }
    }
    fn visit_no_substitution_template_lit(
        &mut self,
        node: &'cx ast::NoSubstitutionTemplateLit,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => {
                bolt_ts_ast_visitor::visit_no_substitution_template_lit(self, node)
            }
            EmitTarget::Js => {
                let content = get_source_text_from_source(&self.origin, node.span).to_string();
                self.emit_token_value(TokenKind::NoSubstitutionTemplate, &content);
            }
            EmitTarget::Both => {
                let content = get_source_text_from_source(&self.origin, node.span).to_string();
                self.emit_token_value(TokenKind::NoSubstitutionTemplate, &content);
            }
        }
    }
    fn visit_expr(&mut self, node: &'cx ast::Expr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_expr(self, node),
            EmitTarget::Js => {
                use ast::ExprKind::*;
                match node.kind {
                    Assign(n) => self.visit_assign_expr(n),
                    Bin(n) => self.visit_bin_expr(n),
                    Omit(_) => {}
                    Paren(n) => self.visit_paren_expr(n),
                    This(n) => self.visit_this_expr(n),
                    Ident(n) => self.visit_ident(n),
                    BoolLit(n) => {
                        if n.val {
                            self.emit_token(TokenKind::True);
                        } else {
                            self.emit_token(TokenKind::False);
                        }
                    }
                    NullLit(_) => self.emit_token(TokenKind::Null),
                    NumLit(n) => self.visit_num_lit(n),
                    BigIntLit(n) => self.visit_big_int_lit(n),
                    RegExpLit(n) => self.visit_reg_exp_lit(n),
                    StringLit(n) => self.visit_string_lit(n),
                    NoSubstitutionTemplateLit(n) => self.visit_no_substitution_template_lit(n),
                    ArrayLit(n) => self.visit_array_lit(n),
                    ObjectLit(n) => self.visit_object_lit(n),
                    Void(n) => {
                        self.emit_token(TokenKind::Void);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Typeof(n) => {
                        self.emit_token(TokenKind::Typeof);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Super(_) => self.emit_token(TokenKind::Super),
                    EleAccess(n) => {
                        self.visit_expr(n.expr);
                        self.emit_token(TokenKind::LBracket);
                        self.visit_expr(n.arg);
                        self.emit_token(TokenKind::RBracket);
                    }
                    PropAccess(n) => self.visit_prop_access_expr(n),
                    PostfixUnary(n) => {
                        self.visit_expr(n.expr);
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
                        self.emit_token(TokenKind::DotDotDot);
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
                        self.emit_token(TokenKind::Delete);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Await(n) => {
                        self.emit_token(TokenKind::Await);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Yield(n) => {
                        self.emit_token(TokenKind::Yield);
                        self.js.print().p_whitespace();
                        if n.asterisk.is_some() {
                            self.emit_token(TokenKind::Asterisk);
                            self.js.print().p_whitespace();
                        }
                        if let Some(expr) = n.expr {
                            self.visit_expr(expr);
                        }
                    }
                    NewMetaProperty(n) => {
                        self.emit_token(TokenKind::New);
                        self.emit_token(TokenKind::Dot);
                        self.visit_ident(n.name);
                    }
                    As(n) => {
                        self.visit_expr(n.expr);
                        self.sync_comment_idx(n.span.hi());
                    }
                    Satisfies(n) => {
                        self.visit_expr(n.expr);
                        self.sync_comment_idx(n.span.hi());
                    }
                    NonNull(n) => {
                        self.visit_expr(n.expr);
                        self.sync_comment_idx(n.span.hi());
                    }
                    TyAssertion(n) => {
                        self.sync_comment_idx(n.expr.span().lo());
                        if matches!(n.expr.kind, ast::ExprKind::ObjectLit(_)) {
                            self.js.print().p_token(TokenKind::LParen);
                            self.visit_expr(n.expr);
                            self.js.print().p_token(TokenKind::RParen);
                        } else {
                            self.visit_expr(n.expr);
                        }
                    }
                    ExprWithTyArgs(n) => self.visit_expr_with_ty_args(n),
                    Import(n) => self.visit_import_expression(n),
                }
            }
            EmitTarget::Both => {
                use ast::ExprKind::*;
                match node.kind {
                    Assign(n) => self.visit_assign_expr(n),
                    Bin(n) => self.visit_bin_expr(n),
                    Omit(_) => {}
                    Paren(n) => self.visit_paren_expr(n),
                    This(n) => self.visit_this_expr(n),
                    Ident(n) => self.visit_ident(n),
                    BoolLit(n) => {
                        if n.val {
                            self.emit_token(TokenKind::True);
                        } else {
                            self.emit_token(TokenKind::False);
                        }
                    }
                    NullLit(_) => self.emit_token(TokenKind::Null),
                    NumLit(n) => self.visit_num_lit(n),
                    BigIntLit(n) => self.visit_big_int_lit(n),
                    RegExpLit(n) => self.visit_reg_exp_lit(n),
                    StringLit(n) => self.visit_string_lit(n),
                    NoSubstitutionTemplateLit(n) => self.visit_no_substitution_template_lit(n),
                    ArrayLit(n) => self.visit_array_lit(n),
                    ObjectLit(n) => self.visit_object_lit(n),
                    Void(n) => {
                        self.emit_token(TokenKind::Void);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Typeof(n) => {
                        self.emit_token(TokenKind::Typeof);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Super(_) => self.emit_token(TokenKind::Super),
                    EleAccess(n) => {
                        self.visit_expr(n.expr);
                        self.emit_token(TokenKind::LBracket);
                        self.visit_expr(n.arg);
                        self.emit_token(TokenKind::RBracket);
                    }
                    PropAccess(n) => self.visit_prop_access_expr(n),
                    PostfixUnary(n) => {
                        self.visit_expr(n.expr);
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
                        self.visit_expr(n.expr);
                    }
                    TaggedTemplate(n) => {
                        self.visit_expr(n.tag);
                        if let Some(ty_args) = n.ty_args {
                            self.with_target(EmitTarget::Declaration, |this| {
                                for ty in ty_args.list {
                                    this.visit_ty(ty);
                                }
                            });
                        }
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
                        self.emit_token(TokenKind::DotDotDot);
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
                        self.emit_token(TokenKind::Delete);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Await(n) => {
                        self.emit_token(TokenKind::Await);
                        self.js.print().p_whitespace();
                        self.visit_expr(n.expr);
                    }
                    Yield(n) => {
                        self.emit_token(TokenKind::Yield);
                        self.js.print().p_whitespace();
                        if n.asterisk.is_some() {
                            self.emit_token(TokenKind::Asterisk);
                            self.js.print().p_whitespace();
                        }
                        if let Some(expr) = n.expr {
                            self.visit_expr(expr);
                        }
                    }
                    NewMetaProperty(n) => {
                        self.emit_token(TokenKind::New);
                        self.emit_token(TokenKind::Dot);
                        self.visit_ident(n.name);
                    }
                    As(n) => {
                        self.visit_expr(n.expr);
                        self.sync_comment_idx(n.span.hi());
                        self.with_target(EmitTarget::Declaration, |this| this.visit_ty(n.ty));
                    }
                    Satisfies(n) => {
                        self.visit_expr(n.expr);
                        self.sync_comment_idx(n.span.hi());
                        self.with_target(EmitTarget::Declaration, |this| this.visit_ty(n.ty));
                    }
                    NonNull(n) => {
                        self.visit_expr(n.expr);
                        self.sync_comment_idx(n.span.hi());
                    }
                    TyAssertion(n) => {
                        self.sync_comment_idx(n.expr.span().lo());
                        if matches!(n.expr.kind, ast::ExprKind::ObjectLit(_)) {
                            self.js.print().p_token(TokenKind::LParen);
                            self.visit_expr(n.expr);
                            self.js.print().p_token(TokenKind::RParen);
                        } else {
                            self.visit_expr(n.expr);
                        }
                        self.with_target(EmitTarget::Declaration, |this| this.visit_ty(n.ty));
                    }
                    ExprWithTyArgs(n) => self.visit_expr_with_ty_args(n),
                    Import(n) => self.visit_import_expression(n),
                }
            }
        }
    }
    fn visit_jsx_ns_name(&mut self, node: &'cx ast::JsxNsName<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_jsx_ns_name(self, node),
            EmitTarget::Js => {
                self.visit_ident(node.ns);
                self.emit_token(TokenKind::Colon);
                self.visit_ident(node.name);
            }
            EmitTarget::Both => {
                self.visit_ident(node.ns);
                self.emit_token(TokenKind::Colon);
                self.visit_ident(node.name);
            }
        }
    }
    fn visit_jsx_expr(&mut self, node: &'cx ast::JsxExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_jsx_expr(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::LBrace);
                if node.dotdotdot_token.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                }
                if let Some(expr) = node.expr {
                    self.visit_expr(expr);
                }
                self.emit_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::LBrace);
                if node.dotdotdot_token.is_some() {
                    self.emit_token(TokenKind::DotDotDot);
                }
                if let Some(expr) = node.expr {
                    self.visit_expr(expr);
                }
                self.emit_token(TokenKind::RBrace);
            }
        }
    }
    fn visit_jsx_frag(&mut self, node: &'cx ast::JsxFrag<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_jsx_frag(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Less);
                self.emit_token(TokenKind::Great);
                for child in node.children {
                    self.visit_jsx_child(*child);
                }
                self.emit_token(TokenKind::LessSlash);
                self.emit_token(TokenKind::Great);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Less);
                self.emit_token(TokenKind::Great);
                for child in node.children {
                    self.visit_jsx_child(*child);
                }
                self.emit_token(TokenKind::LessSlash);
                self.emit_token(TokenKind::Great);
            }
        }
    }
    fn visit_jsx_self_closing_elem(
        &mut self,
        node: &'cx ast::JsxSelfClosingElem<'cx>,
    ) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_jsx_self_closing_elem(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Less);
                self.visit_jsx_tag_name(node.tag_name);
                self.js.print().p_whitespace();
                for attr in node.attrs {
                    self.js.print().p_whitespace();
                    self.visit_jsx_attr(attr);
                }
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Slash);
                self.emit_token(TokenKind::Great);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Less);
                self.visit_jsx_tag_name(node.tag_name);
                if let Some(ty_args) = node.ty_args {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty in ty_args.list {
                            this.visit_ty(ty);
                        }
                    });
                }
                self.js.print().p_whitespace();
                for attr in node.attrs {
                    self.js.print().p_whitespace();
                    self.visit_jsx_attr(attr);
                }
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Slash);
                self.emit_token(TokenKind::Great);
            }
        }
    }
    fn visit_jsx_elem(&mut self, node: &'cx ast::JsxElem<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_jsx_elem(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Less);
                self.visit_jsx_tag_name(node.opening_elem.tag_name);
                self.js.print().p_whitespace();
                for attr in node.opening_elem.attrs {
                    self.js.print().p_whitespace();
                    self.visit_jsx_attr(attr);
                }
                self.emit_token(TokenKind::Great);

                for child in node.children {
                    self.visit_jsx_child(*child);
                }

                self.emit_token(TokenKind::LessSlash);
                self.visit_jsx_tag_name(node.closing_elem.tag_name);
                self.emit_token(TokenKind::Great);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Less);
                self.visit_jsx_tag_name(node.opening_elem.tag_name);
                if let Some(ty_args) = node.opening_elem.ty_args {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty in ty_args.list {
                            this.visit_ty(ty);
                        }
                    });
                }
                self.js.print().p_whitespace();
                for attr in node.opening_elem.attrs {
                    self.js.print().p_whitespace();
                    self.visit_jsx_attr(attr);
                }
                self.emit_token(TokenKind::Great);

                for child in node.children {
                    self.visit_jsx_child(*child);
                }

                self.emit_token(TokenKind::LessSlash);
                self.visit_jsx_tag_name(node.closing_elem.tag_name);
                self.emit_token(TokenKind::Great);
            }
        }
    }
    fn visit_cond_expr(&mut self, node: &'cx ast::CondExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_cond_expr(self, node),
            EmitTarget::Js => {
                self.visit_expr(node.cond);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Question);
                self.js.print().p_whitespace();
                self.visit_expr(node.when_true);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Colon);
                self.js.print().p_whitespace();
                self.visit_expr(node.when_false);
            }
            EmitTarget::Both => {
                self.visit_expr(node.cond);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Question);
                self.js.print().p_whitespace();
                self.visit_expr(node.when_true);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Colon);
                self.js.print().p_whitespace();
                self.visit_expr(node.when_false);
            }
        }
    }
    fn visit_fn_expr(&mut self, node: &'cx ast::FnExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_fn_expr(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::Function);
                if node.asterisk.is_some() {
                    self.emit_token(TokenKind::Asterisk);
                }
                self.js.print().p_whitespace();
                if let Some(name) = node.name {
                    self.visit_ident(name);
                }
                self.emit_params_without_this(node.params);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.body);
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::Function);
                if node.asterisk.is_some() {
                    self.emit_token(TokenKind::Asterisk);
                }
                self.js.print().p_whitespace();
                if let Some(name) = node.name {
                    self.visit_ident(name);
                }
                if let Some(ty_params) = node.ty_params {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty_param in ty_params {
                            this.visit_ty_param(ty_param);
                        }
                    });
                }
                self.emit_fn_params(node.params, false);
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                }
                self.js.print().p_whitespace();
                self.visit_block_stmt(node.body);
            }
        }
    }
    fn visit_call_expr(&mut self, node: &'cx ast::CallExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                self.visit_expr(node.expr);
                self.emit_args(node.args);
            }
            EmitTarget::Declaration => {}
            EmitTarget::Both => {
                self.with_target(EmitTarget::Js, |this| this.visit_call_expr(node));
            }
        }
    }
    fn visit_new_expr(&mut self, node: &'cx ast::NewExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_new_expr(self, node),
            EmitTarget::Js => {
                self.emit_token(TokenKind::New);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
                match node.args {
                    Some(args) => self.emit_args(args),
                    None => {
                        self.js.print().p_token(TokenKind::LParen);
                        self.js.print().p_token(TokenKind::RParen);
                    }
                }
            }
            EmitTarget::Both => {
                self.emit_token(TokenKind::New);
                self.js.print().p_whitespace();
                self.visit_expr(node.expr);
                if let Some(ty_args) = node.ty_args {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty in ty_args.list {
                            this.visit_ty(ty);
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
    }
    fn visit_expr_with_ty_args(&mut self, node: &'cx ast::ExprWithTyArgs<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_expr_with_ty_args(self, node),
            EmitTarget::Js => {
                self.visit_expr(node.expr);
                self.sync_comment_idx(node.span.hi());
            }
            EmitTarget::Both => {
                self.visit_expr(node.expr);
                self.sync_comment_idx(node.span.hi());
                if let Some(ty_args) = node.ty_args {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty in ty_args.list {
                            this.visit_ty(ty);
                        }
                    });
                }
            }
        }
    }
    fn visit_arrow_fn_expr(&mut self, node: &'cx ast::ArrowFnExpr<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_arrow_fn_expr(self, node),
            EmitTarget::Js => {
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
                    self.visit_param_decl(node.params[0]);
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
                        self.visit_expr(expr);
                        self.js.print().p_token(TokenKind::RParen);
                    }
                    ast::ArrowFnExprBody::Block(block) => {
                        let mut non_omitted =
                            block.stmts.iter().filter(|s| !self.stmt_is_omitted(s));
                        let single = non_omitted.next().filter(|_| non_omitted.next().is_none());
                        if let Some(stmt) = single
                            && let ast::StmtKind::Ret(ret) = stmt.kind
                            && let Some(expr) = ret.expr
                        {
                            self.emit_token(TokenKind::LParen);
                            self.visit_expr(expr);
                            self.emit_token(TokenKind::RParen);
                        } else {
                            self.visit_block_stmt(block);
                        }
                    }
                }
            }
            EmitTarget::Both => {
                if node.async_modifier.is_some() {
                    self.emit_token(TokenKind::Async);
                    self.js.print().p_whitespace();
                }
                if let Some(ty_params) = node.ty_params {
                    self.with_target(EmitTarget::Declaration, |this| {
                        for ty_param in ty_params {
                            this.visit_ty_param(ty_param);
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
                    self.visit_param_decl(node.params[0]);
                    self.js.print().p_token(TokenKind::RParen);
                } else {
                    self.emit_fn_params(node.params, false);
                }
                if let Some(ty) = node.ty {
                    self.sync_comment_idx(ty.span().hi());
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ty(ty));
                }
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::EqGreat);
                self.js.print().p_whitespace();

                match node.body {
                    ast::ArrowFnExprBody::Expr(expr) => {
                        self.js.print().p_token(TokenKind::LParen);
                        self.visit_expr(expr);
                        self.js.print().p_token(TokenKind::RParen);
                    }
                    ast::ArrowFnExprBody::Block(block) => {
                        let mut non_omitted =
                            block.stmts.iter().filter(|s| !self.stmt_is_omitted(s));
                        let single = non_omitted.next().filter(|_| non_omitted.next().is_none());
                        if let Some(stmt) = single
                            && let ast::StmtKind::Ret(ret) = stmt.kind
                            && let Some(expr) = ret.expr
                        {
                            for s in block.stmts {
                                if self.stmt_is_omitted(s) {
                                    self.with_target(EmitTarget::Declaration, |this| {
                                        this.visit_stmt(s)
                                    });
                                }
                            }
                            self.emit_token(TokenKind::LParen);
                            self.visit_expr(expr);
                            self.emit_token(TokenKind::RParen);
                        } else {
                            self.visit_block_stmt(block);
                        }
                    }
                }
            }
        }
    }
    fn visit_import_equals_decl(&mut self, node: &'cx ast::ImportEqualsDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                if self.resolver.is_import_equals_namespace_module(node) {
                    return;
                }
                self.emit_token(TokenKind::Var);
                self.js.print().p_whitespace();
                self.visit_ident(node.name);
                self.js.print().p_whitespace();
                self.emit_token(TokenKind::Eq);
                self.js.print().p_whitespace();
                match node.module_reference {
                    ast::ModuleReferenceKind::EntityName(n) => self.visit_entity_name(n),
                    ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                        self.visit_string_lit(n.module_spec())
                    }
                }
            }
            EmitTarget::Declaration => {
                if node.export_modifier.is_some() {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Export);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Import);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ident(node.name);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Eq);
                self.dts.as_mut().unwrap().print().p_whitespace();
                match node.module_reference {
                    ast::ModuleReferenceKind::EntityName(n) => {
                        self.visit_entity_name(n);
                    }
                    ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                        self.visit_string_lit(n.module_spec());
                    }
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                let js_on = !self.resolver.is_import_equals_namespace_module(node);
                if js_on {
                    self.emit_token(TokenKind::Var);
                    self.js.print().p_whitespace();
                }
                if let Some(dts) = &mut self.dts {
                    if node.export_modifier.is_some() {
                        dts.print().p_token(TokenKind::Export);
                        dts.print().p_whitespace();
                    }
                    dts.print().p_token(TokenKind::Import);
                    dts.print().p_whitespace();
                }
                if js_on {
                    self.visit_ident(node.name);
                } else {
                    self.with_target(EmitTarget::Declaration, |this| this.visit_ident(node.name));
                }
                if js_on {
                    self.js.print().p_whitespace();
                    self.emit_token(TokenKind::Eq);
                    self.js.print().p_whitespace();
                }
                dts!(self, print().p_whitespace());
                dts!(self, print().p_token(TokenKind::Eq));
                dts!(self, print().p_whitespace());
                match node.module_reference {
                    ast::ModuleReferenceKind::EntityName(n) => {
                        if js_on {
                            self.visit_entity_name(n);
                        } else {
                            self.with_target(EmitTarget::Declaration, |this| {
                                this.visit_entity_name(n)
                            });
                        }
                    }
                    ast::ModuleReferenceKind::ExternalModuleReference(n) => {
                        if js_on {
                            self.visit_string_lit(n.module_spec());
                        } else {
                            self.with_target(EmitTarget::Declaration, |this| {
                                this.visit_string_lit(n.module_spec())
                            });
                        }
                    }
                }
                dts!(self, print().p_token(TokenKind::Semi));
            }
        }
    }
    fn visit_qualified_name(&mut self, node: &'cx ast::QualifiedName<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_qualified_name(self, node),
            EmitTarget::Js => {
                self.visit_entity_name(node.left);
                self.emit_token(TokenKind::Dot);
                self.visit_ident(node.right);
            }
            EmitTarget::Both => {
                self.target = EmitTarget::Js;
                self.visit_qualified_name(node);
                self.target = EmitTarget::Both;
                if self.dts.is_some() {
                    self.target = EmitTarget::Declaration;
                    self.visit_qualified_name(node);
                    self.target = EmitTarget::Both;
                }
            }
        }
    }
    fn visit_entity_name(&mut self, node: &'cx ast::EntityName<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {
                use ast::EntityNameKind::*;
                match &node.kind {
                    Ident(n) => self.visit_ident(n),
                    Qualified(n) => self.visit_qualified_name(n),
                }
            }
            EmitTarget::Declaration => {
                use ast::EntityNameKind::*;
                match &node.kind {
                    Ident(n) => self.visit_ident(n),
                    Qualified(q) => {
                        self.visit_entity_name(q.left);
                        self.dts.as_mut().unwrap().print().p_token(TokenKind::Dot);
                        self.visit_ident(q.right);
                    }
                }
            }
            EmitTarget::Both => {
                use ast::EntityNameKind::*;
                match &node.kind {
                    Ident(n) => self.visit_ident(n),
                    Qualified(q) => {
                        self.visit_entity_name(q.left);
                        self.emit_token(TokenKind::Dot);
                        dts!(self, print().p_token(TokenKind::Dot));
                        self.visit_ident(q.right);
                    }
                }
            }
        }
    }
    fn visit_ty(&mut self, node: &'cx ast::Ty<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_ty(self, node),
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_ty(node));
            }
        }
    }
    fn visit_refer_ty(&mut self, n: &'cx ast::ReferTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.visit_entity_name(n.name);
                self.emit_type_arguments(n.ty_args);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_refer_ty(n));
            }
        }
    }
    fn visit_array_ty(&mut self, node: &'cx ast::ArrayTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.visit_ty(node.ele);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBracket);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBracket);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_array_ty(node));
            }
        }
    }
    fn visit_indexed_access_ty(&mut self, node: &'cx ast::IndexedAccessTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.visit_ty(node.ty);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBracket);
                self.visit_ty(node.index_ty);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBracket);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_indexed_access_ty(node)
                });
            }
        }
    }
    fn visit_fn_ty(&mut self, node: &'cx ast::FnTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.emit_type_parameters(node.ty_params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::EqGreat);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.ty);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_fn_ty(node));
            }
        }
    }
    fn visit_ctor_ty(&mut self, node: &'cx ast::CtorTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts.as_mut().unwrap().print().p_token(TokenKind::New);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.emit_type_parameters(node.ty_params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::EqGreat);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.ty);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_ctor_ty(node));
            }
        }
    }
    fn visit_object_lit_ty(&mut self, node: &'cx ast::ObjectLitTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBrace);
                if !node.members.is_empty() {
                    self.dts.as_mut().unwrap().increment_indent();
                    self.dts.as_mut().unwrap().print().p_newline();
                    self.emit_list_indexed(
                        node.members,
                        |this, elem, _| {
                            use ast::ObjectTyMemberKind::*;
                            match elem.kind {
                                IndexSig(n) => this.visit_index_sig_decl(n),
                                Prop(n) => this.visit_prop_signature(n),
                                Method(n) => this.visit_method_signature(n),
                                CallSig(n) => this.visit_call_sig_decl(n),
                                CtorSig(n) => this.visit_ctor_sig_decl(n),
                                Setter(n) => this.visit_setter_decl(n),
                                Getter(n) => this.visit_getter_decl(n),
                            }
                        },
                        |this, _, _| {
                            this.dts.as_mut().unwrap().print().p_newline();
                        },
                    );
                    self.dts.as_mut().unwrap().decrement_indent();
                    self.dts.as_mut().unwrap().print().p_newline();
                }
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_object_lit_ty(node)
                });
            }
        }
    }
    fn visit_lit_ty(&mut self, node: &'cx ast::LitTy) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                use ast::LitTyKind::*;
                match &node.kind {
                    Void => self.dts.as_mut().unwrap().print().p_token(TokenKind::Void),
                    Null => self.dts.as_mut().unwrap().print().p_token(TokenKind::Null),
                    True => self.dts.as_mut().unwrap().print().p_token(TokenKind::True),
                    False => self.dts.as_mut().unwrap().print().p_token(TokenKind::False),
                    Undefined => self
                        .dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Undefined),
                    Num(num) => self.dts.as_mut().unwrap().print().p(&num.to_string()),
                    String(atom) => self.emit_string_literal(*atom),
                    BigInt { neg, val } => {
                        if *neg {
                            self.dts.as_mut().unwrap().print().p_token(TokenKind::Minus);
                        }
                        self.dts
                            .as_mut()
                            .unwrap()
                            .emit_atom(self.resolver.atoms(), *val);
                        self.dts.as_mut().unwrap().print().p("n")
                    }
                }
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_lit_ty(node));
            }
        }
    }
    fn visit_named_tuple_ty(&mut self, node: &'cx ast::NamedTupleTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_named_tuple_ty(self, node),
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_named_tuple_ty(node)
                });
            }
        }
    }
    fn visit_tuple_ty(&mut self, node: &'cx ast::TupleTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBracket);
                self.emit_list_indexed(
                    node.tys,
                    |this, item, _| {
                        this.visit_ty(item);
                    },
                    |this, _, _| {
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                    },
                );
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBracket);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_tuple_ty(node));
            }
        }
    }
    fn visit_rest_ty(&mut self, node: &'cx ast::RestTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::DotDotDot);
                self.visit_ty(node.ty);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_rest_ty(node));
            }
        }
    }
    fn visit_cond_ty(&mut self, node: &'cx ast::CondTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.visit_ty(node.check_ty);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Extends);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.extends_ty);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Question);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.true_ty);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.false_ty);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_cond_ty(node));
            }
        }
    }
    fn visit_union_ty(&mut self, n: &'cx ast::UnionTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.emit_list_indexed(
                    n.tys,
                    |this, item, _| {
                        this.visit_ty(item);
                    },
                    |this, _, _| {
                        this.dts.as_mut().unwrap().print().p_whitespace();
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Pipe);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                    },
                );
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_union_ty(n));
            }
        }
    }
    fn visit_intersection_ty(&mut self, node: &'cx ast::IntersectionTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.emit_list_indexed(
                    node.tys,
                    |this, item, _| {
                        this.visit_ty(item);
                    },
                    |this, _, _| {
                        this.dts.as_mut().unwrap().print().p_whitespace();
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Amp);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                    },
                );
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_intersection_ty(node)
                });
            }
        }
    }
    fn visit_typeof_ty(&mut self, node: &'cx ast::TypeofTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Typeof);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_entity_name(node.name);
                self.emit_type_arguments(node.ty_args);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_typeof_ty(node));
            }
        }
    }
    fn visit_mapped_ty(&mut self, node: &'cx ast::MappedTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBrace);
                self.dts.as_mut().unwrap().print().p_newline();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBracket);
                self.visit_ident(node.ty_param.name);
                if let Some(constraint) = node.ty_param.constraint {
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::In);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_ty(constraint);
                }
                if let Some(name_ty) = node.name_ty {
                    self.visit_ty(name_ty);
                }
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBracket);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                if let Some(ty) = node.ty {
                    self.visit_ty(ty);
                }
                self.dts.as_mut().unwrap().print().p_newline();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_mapped_ty(node));
            }
        }
    }
    fn visit_ty_op_ty(&mut self, node: &'cx ast::TypeOp<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                match node.op {
                    ast::TyOpKind::Keyof => {
                        self.dts.as_mut().unwrap().print().p_token(TokenKind::Keyof)
                    }
                    ast::TyOpKind::Readonly => self
                        .dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Readonly),
                    ast::TyOpKind::Unique => self
                        .dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Unique),
                }
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.ty);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_ty_op_ty(node));
            }
        }
    }
    fn visit_ty_param(&mut self, node: &'cx ast::TyParam<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.visit_ident(node.name);
                if let Some(constraint) = node.constraint {
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Extends);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_ty(constraint);
                }
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_ty_param(node));
            }
        }
    }
    fn visit_pred_ty(&mut self, node: &'cx bolt_ts_ast::PredTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                if node.asserts.is_some() {
                    self.dts.as_mut().unwrap().print().p("asserts");
                    self.dts.as_mut().unwrap().print().p_whitespace();
                }
                match node.name {
                    ast::PredTyName::Ident(ident) => self.visit_ident(ident),
                    ast::PredTyName::This(_) => {
                        self.dts.as_mut().unwrap().print().p("this");
                    }
                }
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts.as_mut().unwrap().print().p("is");
                self.dts.as_mut().unwrap().print().p_whitespace();
                if let Some(ty) = node.ty {
                    self.visit_ty(ty);
                }
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_pred_ty(node));
            }
        }
    }
    fn visit_paren_ty(&mut self, node: &'cx ast::ParenTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.visit_ty(node.ty);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_paren_ty(node));
            }
        }
    }
    fn visit_infer_ty(&mut self, node: &'cx ast::InferTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Infer);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ident(node.ty_param.name);
                if let Some(constraint) = node.ty_param.constraint {
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Extends);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_ty(constraint);
                }
                if let Some(default) = node.ty_param.default {
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Eq);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_ty(default);
                }
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_infer_ty(node));
            }
        }
    }
    fn visit_intrinsic_ty(&mut self, node: &'cx ast::IntrinsicTy) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_intrinsic_ty(self, node),
            EmitTarget::Both => {}
        }
    }
    fn visit_nullable_ty(&mut self, node: &'cx ast::NullableTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_nullable_ty(self, node),
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_nullable_ty(node));
            }
        }
    }
    fn visit_template_lit_ty(&mut self, node: &'cx ast::TemplateLitTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                visit_template_head_ty(self, node.head);
                for span in node.spans {
                    self.visit_template_span_ty(span)
                }
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_template_lit_ty(node)
                });
            }
        }
    }
    fn visit_this_ty(&mut self, node: &'cx ast::ThisTy) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => bolt_ts_ast_visitor::visit_this_ty(self, node),
            EmitTarget::Both => {}
        }
    }
    fn visit_interface_decl(&mut self, node: &'cx ast::InterfaceDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.emit_export_modifier_if_needed(contain_export_modifier(node.modifiers));
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::Interface);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .emit_atom(self.resolver.atoms(), node.name.name);
                self.emit_type_parameters(node.ty_params);
                if let Some(extends) = node.extends {
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Extends);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    for item in extends.list {
                        self.visit_refer_ty(item);
                    }
                }
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBrace);
                if !node.members.is_empty() {
                    self.dts.as_mut().unwrap().increment_indent();
                    self.dts.as_mut().unwrap().print().p_newline();
                    self.emit_list_indexed(
                        node.members,
                        |this, elem, _| {
                            bolt_ts_ast_visitor::visit_object_ty_member(this, elem);
                        },
                        |this, _, _| {
                            this.dts.as_mut().unwrap().print().p_newline();
                        },
                    );
                    self.dts.as_mut().unwrap().decrement_indent();
                    self.dts.as_mut().unwrap().print().p_newline();
                }
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_interface_decl(node)
                });
            }
        }
    }
    fn visit_type_alias_decl(&mut self, node: &'cx ast::TypeAliasDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Type);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .emit_atom(self.resolver.atoms(), node.name.name);
                self.emit_type_parameters(node.ty_params);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Eq);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.ty);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_type_alias_decl(node)
                });
            }
        }
    }
    fn visit_index_sig_decl(&mut self, node: &'cx ast::IndexSigDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBracket);
                self.visit_binding(node.key);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.visit_ty(node.key_ty);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBracket);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.visit_ty(node.ty);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_index_sig_decl(node)
                });
            }
        }
    }
    fn visit_prop_signature(&mut self, node: &'cx ast::PropSignature<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.visit_prop_name(node.name);
                if node.question.is_some() {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Question);
                }
                if let Some(ty) = node.ty {
                    self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                    self.dts.as_mut().unwrap().print().p_whitespace();
                    self.visit_ty(ty);
                }
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_prop_signature(node)
                });
            }
        }
    }
    fn visit_method_signature(&mut self, node: &'cx ast::MethodSignature<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.visit_prop_name(node.name);
                if node.question.is_some() {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Question);
                }
                self.emit_type_parameters(node.ty_params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);

                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.emit_ret_ty(node.ty);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_method_signature(node)
                });
            }
        }
    }
    fn visit_call_sig_decl(&mut self, node: &'cx ast::CallSigDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.emit_ret_ty(node.ty);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_call_sig_decl(node)
                });
            }
        }
    }
    fn visit_ctor_sig_decl(&mut self, node: &'cx ast::CtorSigDecl<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => {}
            EmitTarget::Declaration => {
                self.dts.as_mut().unwrap().print().p_token(TokenKind::New);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.emit_type_parameters(node.ty_params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LParen);
                self.emit_parameters(node.params);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RParen);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Colon);
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.emit_ret_ty(node.ty);
                self.dts.as_mut().unwrap().print().p_token(TokenKind::Semi);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_ctor_sig_decl(node)
                });
            }
        }
    }
    fn visit_module_block(&mut self, node: &'cx ast::ModuleBlock<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => bolt_ts_ast_visitor::visit_module_block(self, node),
            EmitTarget::Declaration => {
                self.dts.as_mut().unwrap().print().p_whitespace();
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBrace);
                let saved_flags = self.flags;
                // TODO: condition for `STRIP_EXPORT_MODIFIER`
                self.flags
                    .insert(EmitDeclarationFlags::STRIP_EXPORT_MODIFIER);
                self.flags.remove(EmitDeclarationFlags::NEED_DECLARE);
                self.dts.as_mut().unwrap().increment_indent();
                self.dts.as_mut().unwrap().print().p_newline();
                self.emit_list_indexed(
                    node.stmts,
                    |this, stmt, _| this.visit_stmt(stmt),
                    |this, _, _| this.dts.as_mut().unwrap().print().p_newline(),
                );
                self.dts.as_mut().unwrap().decrement_indent();
                self.dts.as_mut().unwrap().print().p_newline();
                self.flags = saved_flags;
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_module_block(node)
                });
            }
        }
    }
    fn visit_array_pat(&mut self, node: &'cx ast::ArrayPat<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => bolt_ts_ast_visitor::visit_array_pat(self, node),
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBracket);
                self.emit_list_indexed(
                    node.elems,
                    |this, item, _| match item.kind {
                        ast::ArrayBindingElemKind::Omit(_) => {}
                        ast::ArrayBindingElemKind::Binding(item) => {
                            this.visit_array_binding(item);
                        }
                    },
                    |this, _, _| {
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                    },
                );
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBracket);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_array_pat(node));
            }
        }
    }
    fn visit_object_pat(&mut self, n: &'cx ast::ObjectPat<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => bolt_ts_ast_visitor::visit_object_pat(self, n),
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBrace);
                self.emit_list_indexed(
                    n.elems,
                    |this, item, _| this.visit_object_binding_elem(item),
                    |this, _, _| {
                        this.dts.as_mut().unwrap().print().p_token(TokenKind::Comma);
                        this.dts.as_mut().unwrap().print().p_whitespace();
                    },
                );
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBrace);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| this.visit_object_pat(n));
            }
        }
    }
    fn visit_computed_prop_name(&mut self, node: &'cx ast::ComputedPropName<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => bolt_ts_ast_visitor::visit_computed_prop_name(self, node),
            EmitTarget::Declaration => {
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::LBracket);
                self.visit_expr(node.expr);
                self.dts
                    .as_mut()
                    .unwrap()
                    .print()
                    .p_token(TokenKind::RBracket);
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_computed_prop_name(node)
                });
            }
        }
    }
    fn visit_template_span_ty(&mut self, node: &'cx ast::TemplateSpanTy<'cx>) -> Self::Result {
        match self.target {
            EmitTarget::Js => bolt_ts_ast_visitor::visit_template_span_ty(self, node),
            EmitTarget::Declaration => {
                self.visit_ty(node.ty);
                self.dts
                    .as_mut()
                    .unwrap()
                    .emit_atom(self.resolver.atoms(), node.text);
                if node.is_tail {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::RBrace);
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Backtick);
                } else {
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::RBrace);
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::Dollar);
                    self.dts
                        .as_mut()
                        .unwrap()
                        .print()
                        .p_token(TokenKind::LBrace);
                }
            }
            EmitTarget::Both => {
                self.with_target(EmitTarget::Declaration, |this| {
                    this.visit_template_span_ty(node)
                });
            }
        }
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

fn visit_template_head_ty(v: &mut ModuleEmitter, node: &ast::TemplateHead) {
    v.dts.as_mut().unwrap().print().p_token(TokenKind::Backtick);
    v.dts
        .as_mut()
        .unwrap()
        .emit_atom(v.resolver.atoms(), node.text);
    v.dts.as_mut().unwrap().print().p_dollar_and_brace();
}

fn visit_nested_module_declaration_inner<'cx>(
    v: &mut ModuleEmitter<'cx, '_>,
    node: &'cx ast::NestedModuleDecl<'cx>,
) {
    v.dts.as_mut().unwrap().print().p_token(TokenKind::Dot);
    v.visit_ident(node.name);
    match node.block {
        ast::NestedModuleBlock::Nested(decl) => visit_nested_module_declaration_inner(v, decl),
        ast::NestedModuleBlock::Block(block) => v.visit_module_block(block),
    }
}
