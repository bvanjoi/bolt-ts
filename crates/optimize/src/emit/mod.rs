mod print;

use std::borrow::Cow;

use bolt_ts_ast as ast;
use bolt_ts_atom::{Atom, AtomIntern};
use rustc_hash::FxHashSet;

use crate::{emit::print::PPrint, ir, lowering::LoweringResult};

#[derive(Clone, Copy)]
pub struct EmitterOptions {
    indent: u32,
}

bolt_ts_utils::index! {
    ScopeID
}

pub fn emit_js(atoms: &AtomIntern, ir: &LoweringResult) -> String {
    let emitter = Emitter::new(atoms);
    let mut js_emitter = JSEmitter {
        emitter,
        ns_names: FxHashSet::default(),
        scope: ScopeID::root(),
        max_scope: ScopeID::root(),
        graph_arena: &ir.graph_arena,
        current_graph: ir.entry_graph,
        nodes: &ir.nodes,
    };
    js_emitter.emit_root(ir.entry_graph)
}

pub struct Emitter<'cx> {
    atoms: &'cx AtomIntern,
    options: EmitterOptions,
    content: PPrint,
}

impl<'cx> Emitter<'cx> {
    pub fn new(atoms: &'cx AtomIntern) -> Self {
        Self {
            atoms,
            options: EmitterOptions { indent: 2 },
            content: PPrint::new(1024),
        }
    }

    #[inline]
    pub fn atom(&self, atom: Atom) -> &'static str {
        self.atoms.get(atom)
    }

    #[inline]
    pub fn print(&mut self) -> &mut PPrint {
        &mut self.content
    }

    #[inline]
    pub fn emit_atom(&mut self, atom: Atom) {
        let name = self.atom(atom);
        self.print().p(name);
    }
}

struct JSEmitter<'cx, 'ir> {
    emitter: Emitter<'cx>,
    ns_names: FxHashSet<(ScopeID, bolt_ts_atom::Atom)>,
    scope: ScopeID,
    max_scope: ScopeID,
    nodes: &'ir ir::Nodes,
    graph_arena: &'ir ir::GraphArena,
    current_graph: ir::GraphID,
}

impl<'ir> JSEmitter<'_, 'ir> {
    fn next_scope(&mut self) -> ScopeID {
        let scope = self.max_scope;
        self.max_scope = self.max_scope.next();
        scope
    }

    fn emit_root(&mut self, entry: ir::GraphID) -> String {
        self.emit_program(entry);
        self.emitter.print().take_content()
    }

    fn graph(&self, id: ir::GraphID) -> &'ir ir::Graph {
        self.graph_arena.get(id)
    }

    fn emit_basic_block(&mut self, graph: ir::GraphID, id: ir::BasicBlockID) {
        let saved = self.current_graph;
        self.current_graph = graph;

        let block = self.graph(graph).get_basic_block(id);
        self.emit_stmts(block.stmts());

        self.current_graph = saved;
    }

    fn emit_basic_block_with_brace(&mut self, graph: ir::GraphID, id: ir::BasicBlockID) {
        let saved = self.current_graph;
        self.current_graph = graph;

        self.emitter.print().p_l_brace();
        let block = self.graph(graph).get_basic_block(id);
        if !block.stmts().is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_stmts(block.stmts());
        if !block.stmts().is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();

        self.current_graph = saved;
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

    fn emit_program(&mut self, root: ir::GraphID) {
        self.emit_basic_block(root, ir::BasicBlockID::ENTRY);
    }

    fn emit_stmts(&mut self, stmts: &[ir::Stmt]) {
        self.emit_list(
            stmts,
            |this, item| this.emit_stmt(*item),
            |this, _| {
                this.emitter.content.p_newline();
            },
        )
    }

    fn emit_var_stmt(&mut self, var: ir::VarStmtID) {
        self.emitter.print().p("var");
        self.emitter.print().p_whitespace();
        let decls = self.nodes.get_var_stmt(&var).decls();
        self.emit_var_decls(decls);
        self.emitter.print().p_semi();
    }

    fn emit_var_decls(&mut self, decls: &'ir [ir::VarDeclID]) {
        self.emit_list(
            decls,
            |this, decl| this.emit_var_decl(*decl),
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_whitespace();
            },
        );
    }

    fn emit_var_decl(&mut self, decl: ir::VarDeclID) {
        let decl = self.nodes.get_var_decl(&decl);
        self.emit_binding(decl.name());
        if let Some(init) = decl.init() {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.emit_expr(init);
        }
    }

    fn emit_ident(&mut self, ident: ir::IdentID) {
        let ident = self.nodes.get_ident(&ident);
        let content = self.emitter.atoms.get(ident.name());
        self.emitter.print().p(content);
    }

    fn emit_num_lit(&mut self, num: ir::NumLitID) {
        let num = self.nodes.get_num_lit(&num);
        self.emitter.print().p(&num.val().to_string())
    }

    fn emit_string_lit(&mut self, s: ir::StringLitID) {
        let s = self.nodes.get_string_lit(&s);
        let val = s.val();
        if s.is_template() {
            self.emitter.print().p("`");
            let content = self.emitter.atoms.get(val);
            self.emitter.print().p(content);
            self.emitter.print().p("`");
        } else {
            self.emit_as_string(val);
        }
    }

    fn emit_as_string(&mut self, val: Atom) {
        let s = self.emitter.atoms.get(val);
        self.emitter.print().p("'");
        for c in s.chars() {
            match c {
                '\'' => self.emitter.print().p("\\'"),
                _ => self.emitter.print().content.push(c),
            }
        }
        self.emitter.print().p("'");
    }

    fn emit_prop_name(&mut self, name: ir::PropName) {
        match name {
            ir::PropName::Ident(id) => self.emit_ident(id),
            ir::PropName::NumLit(id) => self.emit_num_lit(id),
            ir::PropName::StringLit(id) => self.emit_string_lit(id),
            ir::PropName::Computed(id) => {
                self.emitter.print().p_l_bracket();
                let name = self.nodes.get_computed_prop_name(&id);
                self.emit_expr(name.expr());
                self.emitter.print().p_r_bracket();
            }
        }
    }

    fn emit_object_binding_elem(&mut self, id: ir::ObjectBindingElemID) {
        let elem = self.nodes.get_object_binding_elem(&id);
        if elem.dotdotdot().is_some() {
            self.emitter.print().p_dot_dot_dot();
        }
        match elem.name() {
            ir::ObjectBindingName::Shorthand(ident) => {
                self.emit_ident(ident);
            }
            ir::ObjectBindingName::Prop { prop_name, name } => {
                self.emit_prop_name(prop_name);
                self.emitter.print().p(":");
                self.emitter.print().p_whitespace();
                self.emit_binding(name);
            }
        }
        if let Some(init) = elem.init() {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.emit_expr(init);
        }
    }

    fn emit_array_binding_elem(&mut self, elem: &ir::ArrayBindingElem) {
        match elem {
            ir::ArrayBindingElem::Omit(_) => {}
            ir::ArrayBindingElem::Binding(n) => {
                let n = self.nodes.get_array_binding(n);
                if n.dotdotdot().is_some() {
                    self.emitter.print().p_dot_dot_dot();
                }
                self.emit_binding(n.name());
                if let Some(init) = n.init() {
                    self.emitter.print().p_whitespace();
                    self.emitter.print().p_eq();
                    self.emitter.print().p_whitespace();
                    self.emit_expr(init);
                }
            }
        }
    }

    fn emit_binding(&mut self, binding: ir::Binding) {
        match binding {
            ir::Binding::Ident(n) => self.emit_ident(n),
            ir::Binding::ObjectPat(n) => {
                self.emitter.print().p_l_brace();
                let pat = self.nodes.get_object_pat(&n);
                self.emit_list(
                    pat.elems(),
                    |this, item| this.emit_object_binding_elem(*item),
                    |this, _| {
                        this.emitter.content.p_comma();
                        this.emitter.content.p_whitespace();
                    },
                );
                self.emitter.print().p_r_brace();
            }
            ir::Binding::ArrayPat(n) => {
                self.emitter.print().p_l_bracket();
                let pat = self.nodes.get_array_pat(&n);
                self.emit_list(
                    pat.elems(),
                    |this, item| {
                        this.emit_array_binding_elem(item);
                    },
                    |this, _| {
                        this.emitter.content.p_comma();
                        this.emitter.content.p_whitespace();
                    },
                );
                self.emitter.print().p_r_bracket();
            }
        };
    }

    fn emit_fn_decl(&mut self, f: ir::FnDeclID) {
        let f = self.nodes.get_fn_decl(&f);
        if f.modifiers()
            .is_some_and(|m| m.flags().contains(ast::ModifierKind::Async))
        {
            self.emitter.print().p("async");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("function");
        self.emitter.print().p_whitespace();
        if let Some(name) = f.name() {
            self.emit_ident(name);
        }
        self.emit_params(f.params());
        self.emitter.print().p_whitespace();

        self.emit_basic_block_with_brace(f.body(), ir::BasicBlockID::ENTRY);
    }

    fn emit_params(&mut self, params: &[ir::ParamDeclID]) {
        self.emitter.print().p_l_paren();
        self.emit_list(
            params,
            |this, item| this.emit_param(*item),
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
    }

    fn emit_param(&mut self, param: ir::ParamDeclID) {
        let param = self.nodes.get_param_decl(&param);
        if param.dotdotdot().is_some() {
            self.emitter.print().p_dot_dot_dot();
        }
        self.emit_binding(param.name());
        if let Some(init) = param.init() {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.emit_expr(init);
        }
    }

    fn emit_block_stmt(&mut self, block: ir::BlockStmtID) {
        let block = self.nodes.get_block_stmt(&block);
        self.emitter.print().p_l_brace();
        if !block.stmts().is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_stmts(block.stmts());
        if !block.stmts().is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn emit_if_stmt(&mut self, stmt: ir::IfStmtID) {
        let stmt = self.nodes.get_if_stmt(&stmt);
        self.emitter.print().p("if");
        self.emitter.print().p_whitespace();
        // test
        self.emitter.print().p_l_paren();
        self.emit_expr(stmt.expr());
        self.emitter.print().p_r_paren();
        self.emitter.print().p_whitespace();
        // block
        self.emit_basic_block(self.current_graph, stmt.then());
        // else
        if let Some(else_then) = stmt.else_then() {
            self.emitter.print().p_whitespace();
            self.emitter.print().p("else");
            self.emitter.print().p_whitespace();
            self.emit_basic_block(self.current_graph, else_then);
        }
        self.emitter.print().p_newline();
    }

    fn emit_ret_stmt(&mut self, ret: ir::RetStmtID) {
        let ret = self.nodes.get_ret_stmt(&ret);
        self.emitter.print().p("return");
        self.emitter.print().p_whitespace();
        if let Some(expr) = ret.expr() {
            self.emit_expr(expr);
        }
    }

    fn emit_class_extends_clause(&mut self, extends: ir::ClassExtendsClauseID) {
        let extends = self.nodes.get_class_extends_clause(&extends);
        self.emitter.print().p("extends");
        self.emitter.print().p_whitespace();
        self.emit_expr(extends.expr());
        self.emitter.print().p_whitespace();
    }

    fn emit_class_elem(&mut self, elem: ir::ClassElem) {
        match elem {
            ir::ClassElem::PropElem(id) => self.emit_class_prop_elem(id),
            ir::ClassElem::MethodElem(id) => self.emit_class_method_elem(id),
            ir::ClassElem::StaticBlock(id) => self.emit_class_static_block(id),
            ir::ClassElem::Ctor(id) => self.emit_class_ctor(id),
            ir::ClassElem::Getter(id) => self.emit_getter_decl(id),
            ir::ClassElem::Setter(id) => self.emit_setter_decl(id),
        }
    }

    fn emit_getter_decl(&mut self, elem: ir::GetterDeclID) {
        let elem = self.nodes.get_getter_decl(&elem);
        if let Some(mods) = elem.modifiers()
            && mods.flags().contains(ast::ModifierKind::Static)
        {
            self.emitter.print().p("static");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("get");
        self.emitter.print().p_whitespace();
        self.emit_prop_name(elem.name());
        self.emit_params(&[]);
        self.emitter.print().p_whitespace();
        self.emit_block_stmt(elem.body());
    }

    fn emit_setter_decl(&mut self, elem: ir::SetterDeclID) {
        let elem = self.nodes.get_setter_decl(&elem);
        if let Some(mods) = elem.modifiers()
            && mods.flags().contains(ast::ModifierKind::Static)
        {
            self.emitter.print().p("static");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("set");
        self.emitter.print().p_whitespace();
        self.emit_prop_name(elem.name());
        self.emit_params(elem.params());
        self.emitter.print().p_whitespace();
        self.emit_block_stmt(elem.body());
    }

    fn emit_class_ctor(&mut self, elem: ir::ClassCtorID) {
        let ctor = self.nodes.get_class_ctor(&elem);
        self.emitter.print().p("constructor");
        self.emit_params(ctor.params());
        self.emitter.print().p_whitespace();

        let body = ctor.body();
        self.emitter.print().p_l_brace();
        self.emitter.print().indent += self.emitter.options.indent;

        let block = self.nodes.get_block_stmt(&body);

        let has_block_stmt = !block.stmts().is_empty()
            && ctor.params().iter().any(|param| {
                let param = self.nodes.get_param_decl(param);
                param.dotdotdot().is_none()
                    && param
                        .modifiers()
                        .is_some_and(|ms| ms.flags().contains(ast::ModifierKind::Public))
            });

        if has_block_stmt {
            self.emitter.print().p_newline();
        }

        let last_super_call = block.stmts().iter().rev().position(|stmt| {
            if let ir::Stmt::Expr(expr) = stmt
                && let expr = self.nodes.get_expr_stmt(expr)
                && let ir::Expr::Call(call) = expr.expr()
                && let call = self.nodes.get_call_expr(&call)
                && let ir::Expr::Super(_) = call.callee()
            {
                return true;
            }
            false
        });

        let (prev_stmts, after_stmts) = if let Some(last_super_call) = last_super_call {
            block.stmts().split_at(last_super_call + 1)
        } else {
            let after_stmts: &[ir::Stmt] = &[];
            (block.stmts(), after_stmts)
        };

        self.emit_list(
            prev_stmts,
            |this, elem| this.emit_stmt(*elem),
            |this, _| {
                this.emitter.content.p_newline();
            },
        );

        self.emit_list(
            ctor.params(),
            |this, param| {
                let param = self.nodes.get_param_decl(param);
                if param.dotdotdot().is_none()
                    && param
                        .modifiers()
                        .is_some_and(|ms| ms.flags().contains(ast::ModifierKind::Public))
                {
                    this.emitter.content.p_newline();

                    this.emitter.content.p("this");
                    this.emitter.content.p_dot();
                    this.emit_binding(param.name());
                    this.emitter.content.p_whitespace();
                    this.emitter.content.p_eq();
                    this.emitter.content.p_whitespace();
                    this.emit_binding(param.name());
                }
            },
            |this, param| {
                let param = self.nodes.get_param_decl(param);
                if param.dotdotdot().is_none()
                    && param
                        .modifiers()
                        .is_some_and(|ms| ms.flags().contains(ast::ModifierKind::Public))
                {
                    this.emitter.content.p_newline();
                }
            },
        );

        self.emit_list(
            after_stmts,
            |this, elem| this.emit_stmt(*elem),
            |this, _| {
                this.emitter.content.p_newline();
            },
        );

        if has_block_stmt {
            self.emitter.print().p_newline();
        }
        self.emitter.print().indent -= self.emitter.options.indent;
        self.emitter.print().p_r_brace();
    }

    fn emit_class_static_block(&mut self, elem: ir::ClassStaticBlockDeclID) {
        let elem = self.nodes.get_class_static_block_decl(&elem);
        self.emitter.print().p("static");
        self.emitter.print().p_whitespace();

        self.emitter.print().p_l_brace();
        self.emit_block_stmt(elem.body());
        self.emitter.print().p_r_brace();
    }

    fn emit_class_prop_elem(&mut self, elem: ir::ClassPropElemID) {
        let elem = self.nodes.get_class_prop_elem(&elem);
        if let Some(mods) = elem.modifiers() {
            if mods.flags().contains(ast::ModifierKind::Abstract) {
                return;
            }
            if mods.flags().contains(ast::ModifierKind::Static) {
                self.emitter.print().p("static");
                self.emitter.print().p_whitespace();
            }
        }
        self.emit_prop_name(elem.name());
        if let Some(init) = elem.init() {
            self.emitter.print().p_whitespace();
            self.emitter.print().p_eq();
            self.emitter.print().p_whitespace();
            self.emit_expr(init);
        }
    }

    fn emit_class_method_elem(&mut self, elem: ir::ClassMethodElemID) {
        let elem = self.nodes.get_class_method_elem(&elem);
        if let Some(mods) = elem.modifiers()
            && mods.flags().contains(ast::ModifierKind::Static)
        {
            self.emitter.print().p("static");
            self.emitter.print().p_whitespace();
        }
        self.emit_prop_name(elem.name());
        self.emit_params(elem.params());
        self.emitter.print().p_whitespace();
        self.emit_block_stmt(elem.body());
    }

    fn emit_class_decl(&mut self, class: ir::ClassDeclID) {
        let class = self.nodes.get_class_decl(&class);
        self.emitter.print().p("class");
        self.emitter.print().p_whitespace();
        if let Some(ident) = class.name() {
            self.emit_ident(ident);
            self.emitter.print().p_whitespace();
            let name = self.nodes.get_ident(&ident).name();
            self.ns_names.insert((self.scope, name));
        }
        if let Some(extends) = class.extends() {
            self.emit_class_extends_clause(extends);
        }
        self.emitter.print().p_l_brace();
        if !class.elems().is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_list(
            class.elems(),
            |this, elem| {
                this.emit_class_elem(*elem);
            },
            |this, _| {
                this.emitter.content.p_newline();
            },
        );
        if !class.elems().is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn emit_throw_stmt(&mut self, stmt: ir::ThrowStmtID) {
        let stmt = self.nodes.get_throw_stmt(&stmt);
        self.emitter.print().p("throw");
        self.emitter.print().p_whitespace();
        self.emit_expr(stmt.expr());
    }

    fn emit_with_var_fn_wrapper(
        &mut self,
        decl_name: ir::IdentID,
        param_name: &str,
        f: impl FnOnce(&mut Self),
    ) {
        let name = self.nodes.get_ident(&decl_name).name();
        if self.ns_names.insert((self.scope, name)) {
            self.emitter.print().p("var");
            self.emitter.print().p_whitespace();
            self.emit_ident(decl_name);
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
        self.emit_ident(decl_name);
        self.emitter.print().p_r_paren();
        self.emitter.print().p_semi();
    }

    fn emit_module_decl(&mut self, decl: ir::ModuleDeclID) {
        let ns = self.nodes.get_module_decl(&decl);
        if ns
            .modifiers()
            .map(|ms| ms.flags().contains(ast::ModifierKind::Ambient))
            .unwrap_or_default()
        {
            return;
        }

        let block = ns.block();

        // var name
        fn sub_names_of_binding<'cx>(
            this: &JSEmitter,
            binding: ir::Binding,
        ) -> Vec<bolt_ts_atom::Atom> {
            match binding {
                ir::Binding::Ident(n) => vec![this.nodes.get_ident(&n).name()],
                ir::Binding::ObjectPat(n) => this
                    .nodes
                    .get_object_pat(&n)
                    .elems()
                    .iter()
                    .flat_map(
                        |elem| match this.nodes.get_object_binding_elem(elem).name() {
                            ir::ObjectBindingName::Shorthand(ident) => {
                                vec![this.nodes.get_ident(&ident).name()]
                            }
                            ir::ObjectBindingName::Prop { name, .. } => {
                                sub_names_of_binding(this, name)
                            }
                        },
                    )
                    .collect(),
                ir::Binding::ArrayPat(_) => todo!(),
            }
        }

        let block = self.nodes.get_module_block(&block);

        let mut sub_names = block
            .stmts()
            .iter()
            .filter_map(|stmt| match stmt {
                ir::Stmt::Var(v) => Some(
                    self.nodes
                        .get_var_stmt(v)
                        .decls()
                        .iter()
                        .flat_map(|item| {
                            sub_names_of_binding(self, self.nodes.get_var_decl(item).name())
                        })
                        .collect::<Vec<_>>(),
                ),
                ir::Stmt::Class(c) => Some(vec![
                    self.nodes
                        .get_class_decl(c)
                        .name()
                        .map(|name| self.nodes.get_ident(&name).name())
                        .unwrap(),
                ]),
                ir::Stmt::Fn(f) => {
                    if let Some(name) = self.nodes.get_fn_decl(f).name() {
                        Some(vec![self.nodes.get_ident(&name).name()])
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .flatten()
            .map(|name| self.emitter.atoms.get(name))
            .collect::<Vec<_>>();
        sub_names.sort();

        let ident = match ns.name() {
            ir::ModuleName::Ident(ident) => ident,
            ir::ModuleName::StringLit(_) => unreachable!(),
        };
        let mut param_name =
            Cow::Borrowed(self.emitter.atoms.get(self.nodes.get_ident(&ident).name()));
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
            for stmt in block.stmts() {
                this.emitter.content.p_newline();
                this.emit_stmt(*stmt);
                this.emitter.content.p_newline();
                let t = match stmt {
                    ir::Stmt::Var(v) => {
                        let v = this.nodes.get_var_stmt(v);
                        if v.modifiers()
                            .map(|ms| ms.flags().contains(ast::ModifierKind::Export))
                            .unwrap_or_default()
                        {
                            for item in v.decls() {
                                this.emitter.content.p(&param_name);
                                this.emitter.content.p_dot();
                                this.emit_binding(this.nodes.get_var_decl(item).name());
                                this.emitter.content.p_whitespace();
                                this.emitter.content.p_eq();
                                this.emitter.content.p_whitespace();
                                // TODO: fix
                                this.emit_binding(this.nodes.get_var_decl(item).name());
                                this.emitter.content.p_newline();
                            }
                        }
                        continue;
                    }
                    ir::Stmt::Fn(f) => {
                        let f = this.nodes.get_fn_decl(f);
                        let Some(name) = f.name() else {
                            continue;
                        };
                        f.modifiers().map(|ms| (ms, name))
                    }
                    ir::Stmt::Class(c) => {
                        let c = this.nodes.get_class_decl(c);
                        c.modifiers().map(|ms| (ms, c.name().unwrap()))
                    }
                    ir::Stmt::Module(n) => {
                        let n = this.nodes.get_module_decl(n);
                        n.modifiers().map(|ms| {
                            let ident = match n.name() {
                                ir::ModuleName::Ident(ident) => ident,
                                ir::ModuleName::StringLit(_) => unreachable!(),
                            };
                            (ms, ident)
                        })
                    }
                    _ => None,
                };
                let Some((ms, name)) = t else {
                    continue;
                };
                if ms.flags().contains(ast::ModifierKind::Export) {
                    this.emitter.content.p(&param_name);
                    this.emitter.content.p_dot();
                    this.emit_ident(name);
                    this.emitter.content.p_whitespace();
                    this.emitter.content.p_eq();
                    this.emitter.content.p_whitespace();
                    this.emit_ident(name);
                    this.emitter.content.p_semi();
                    this.emitter.content.p_newline();
                }
            }
        });
    }

    fn emit_enum_decl(&mut self, e: ir::EnumDeclID) {
        let e = self.nodes.get_enum_decl(&e);
        if e.modifiers()
            .map(|ms| ms.flags().contains(ast::ModifierKind::Ambient))
            .unwrap_or_default()
        {
            return;
        }
        self.emit_with_var_fn_wrapper(
            e.name(),
            self.emitter
                .atoms
                .get(self.nodes.get_ident(&e.name()).name()),
            |this| {
                for member in e.members() {
                    this.emitter.content.p_newline();
                    this.emit_ident(e.name());
                    this.emitter.content.p_l_bracket();
                    this.emit_ident(e.name());
                    this.emitter.content.p_l_bracket();
                    let member = this.nodes.get_enum_member(member);
                    match member.name() {
                        ir::PropName::Ident(ident) => {
                            this.emit_as_string(self.nodes.get_ident(&ident).name())
                        }
                        ir::PropName::StringLit(lit) => this.emit_string_lit(lit),
                        ir::PropName::NumLit(num) => this.emit_num_lit(num),
                        ir::PropName::Computed(_) => todo!(),
                    }
                    this.emitter.content.p_r_bracket();
                    this.emitter.content.p_whitespace();
                    this.emitter.content.p_eq();
                    this.emitter.content.p_whitespace();
                    if let Some(init) = member.init() {
                        this.emit_expr(init);
                    } else {
                        // todo:
                        let val = 0;
                        this.emitter.content.p(&val.to_string());
                    }
                    this.emitter.content.p_r_bracket();
                    this.emitter.content.p_whitespace();
                    this.emitter.content.p_eq();
                    this.emitter.content.p_whitespace();
                    match member.name() {
                        ir::PropName::Ident(ident) => {
                            this.emit_as_string(self.nodes.get_ident(&ident).name())
                        }
                        ir::PropName::StringLit(lit) => this.emit_string_lit(lit),
                        ir::PropName::NumLit(num) => this.emit_num_lit(num),
                        ir::PropName::Computed(_) => todo!(),
                    }
                }
            },
        )
    }

    fn emit_import_decl(&mut self, n: ir::ImportDeclID) {
        let n = self.nodes.get_import_decl(&n);

        self.emitter.print().p("import");
        self.emitter.print().p_whitespace();
        if let Some(clause) = n.clause() {
            self.emit_import_clause(clause);
        }
        self.emitter.print().p_whitespace();
        self.emitter.print().p("from");
        self.emitter.print().p_whitespace();
        self.emit_string_lit(n.module());
    }

    fn emit_import_clause(&mut self, clause: ir::ImportClauseID) {
        let clause = self.nodes.get_import_clause(&clause);
        if let Some(name) = clause.name() {
            self.emit_ident(name);
            self.emitter.print().p_whitespace();
        } else if let Some(kind) = clause.kind() {
            match kind {
                ir::ImportClauseKind::Specs(specs) => {
                    self.emit_list(
                        specs,
                        |this, spec| this.emit_import_spec(*spec),
                        |this, _| {
                            this.emitter.content.p_comma();
                            this.emitter.content.p_whitespace();
                        },
                    );
                }
                ir::ImportClauseKind::Ns(ns) => self.emit_ns_import(*ns),
            }
        }
    }

    fn emit_ns_import(&mut self, ns: ir::NsImportID) {
        let ns = self.nodes.get_ns_import(&ns);
        self.emitter.print().p_asterisk();
        self.emitter.print().p_whitespace();
        self.emitter.print().p("as");
        self.emitter.print().p_whitespace();
        self.emit_ident(ns.name());
    }

    fn emit_module_export_name(&mut self, n: ir::ModuleExportName) {
        match n {
            ir::ModuleExportName::Ident(ident) => self.emit_ident(ident),
            ir::ModuleExportName::StringLit(lit) => self.emit_string_lit(lit),
        }
    }

    fn emit_shorthand_spec(&mut self, n: ir::ShorthandSpecID) {
        let n = self.nodes.get_shorthand_spec(&n);
        self.emit_ident(n.name());
    }

    fn emit_import_spec(&mut self, spec: ir::ImportSpec) {
        match spec {
            ir::ImportSpec::Shorthand(n) => self.emit_shorthand_spec(n),
            ir::ImportSpec::Named(n) => {
                let n = self.nodes.get_import_named_spec(&n);
                self.emit_module_export_name(n.prop_name());
                self.emitter.print().p_whitespace();
                self.emitter.print().p("as");
                self.emitter.print().p_whitespace();
                self.emit_ident(n.name());
            }
        }
    }

    fn emit_stmt(&mut self, stmt: ir::Stmt) {
        use ir::Stmt::*;
        match stmt {
            Var(id) => self.emit_var_stmt(id),
            Expr(id) => {
                let stmt = self.nodes.get_expr_stmt(&id);
                self.emit_expr(stmt.expr());
                self.emitter.print().p_semi();
            }
            Fn(id) => self.emit_fn_decl(id),
            If(id) => self.emit_if_stmt(id),
            Block(id) => self.emit_block_stmt(id),
            Ret(id) => self.emit_ret_stmt(id),
            Class(id) => self.emit_class_decl(id),
            Throw(id) => self.emit_throw_stmt(id),
            Module(id) => self.emit_module_decl(id),
            Enum(id) => self.emit_enum_decl(id),
            Import(id) => self.emit_import_decl(id),
            Export(id) => self.emit_export_decl(id),
            For(id) => self.emit_for_stmt(id),
            ForOf(id) => self.emit_for_of_stmt(id),
            ForIn(id) => self.emit_for_in_stmt(id),
            Break(id) => self.emit_break_stmt(id),
            Continue(id) => self.emit_continue_stmt(id),
            Try(id) => self.emit_try_stmt(id),
            While(id) => self.emit_while_stmt(id),
            Do(id) => self.emit_do_stmt(id),
            ExportAssign(id) => self.emit_export_assign(id),
            Labeled(id) => self.emit_labeled_stmt(id),
            Empty(id) => self.emit_empty_stmt(id),
            Switch(id) => self.emit_switch_stmt(id),
        }
    }

    fn emit_switch_stmt(&mut self, n: ir::SwitchStmtID) {
        let n = self.nodes.get_switch_stmt(&n);
        self.emitter.print().p("switch");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_paren();
        self.emit_expr(n.expr());
        self.emitter.print().p_r_paren();
        self.emitter.print().p_whitespace();

        self.emitter.print().p_l_brace();
        self.emit_case_block(n.case_block());
        self.emitter.print().p_r_brace();
    }

    fn emit_case_block(&mut self, n: ir::CaseBlockID) {
        let n = self.nodes.get_case_block(&n);
        if !n.clauses().is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_list(
            n.clauses(),
            |this, item| match *item {
                ir::CaseOrDefaultClause::Case(n) => this.emit_case_clause(n),
                ir::CaseOrDefaultClause::Default(n) => this.emit_default_clause(n),
            },
            |this, _| {
                this.emitter.content.p_newline();
            },
        );
        if !n.clauses().is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
    }

    fn emit_case_clause(&mut self, n: ir::CaseClauseID) {
        let n = self.nodes.get_case_clause(&n);
        self.emitter.print().p("case");
        self.emitter.print().p_whitespace();
        self.emit_expr(n.expr());
        self.emitter.print().p_colon();
        if !n.stmts().is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_stmts(n.stmts());
        if !n.stmts().is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
    }

    fn emit_default_clause(&mut self, n: ir::DefaultClauseID) {
        let n = self.nodes.get_default_clause(&n);
        self.emitter.print().p("default");
        self.emitter.print().p_colon();
        if !n.stmts().is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_stmts(n.stmts());
        if !n.stmts().is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
    }

    fn emit_empty_stmt(&mut self, _: ir::EmptyStmtID) {
        self.emitter.print().p_semi();
    }

    fn emit_export_assign(&mut self, n: ir::ExportAssignID) {
        let n = self.nodes.get_export_assign(&n);
        self.emitter.print().p("export default ");
        self.emit_expr(n.expr());
        self.emitter.print().p_semi();
    }

    fn emit_labeled_stmt(&mut self, n: ir::LabeledStmtID) {
        let n = self.nodes.get_labeled_stmt(&n);
        self.emit_ident(n.label());
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.emit_stmt(n.body());
    }

    fn emit_do_stmt(&mut self, n: ir::DoStmtID) {
        let n = self.nodes.get_do_stmt(&n);
        self.emitter.print().p("do");
        self.emitter.print().p_whitespace();
        self.emit_stmt(n.stmt());
        self.emitter.print().p_whitespace();
        self.emitter.print().p("while");
        self.emitter.print().p_whitespace();
        self.emitter.print().p_l_paren();
        self.emit_expr(n.expr());
        self.emitter.print().p_r_paren();
    }

    fn emit_while_stmt(&mut self, n: ir::WhileStmtID) {
        let n = self.nodes.get_while_stmt(&n);

        self.emitter.print().p("while");
        self.emitter.print().p_whitespace();

        self.emitter.print().p_l_paren();
        self.emit_expr(n.expr());
        self.emitter.print().p_r_paren();

        self.emitter.print().p_whitespace();
        self.emit_stmt(n.body());
    }

    fn emit_catch_block(&mut self, n: ir::CatchClauseID) {
        let n = self.nodes.get_catch_clause(&n);
        self.emitter.print().p("catch");
        self.emitter.print().p_whitespace();
        if let Some(var) = n.var() {
            self.emitter.print().p("(");
            self.emit_var_decl(var);
            self.emitter.print().p(")");
        }
        self.emitter.print().p_whitespace();
        self.emit_block_stmt(n.block());
    }

    fn emit_try_stmt(&mut self, n: ir::TryStmtID) {
        let n = self.nodes.get_try_stmt(&n);

        self.emitter.print().p("try");
        self.emitter.print().p_whitespace();
        self.emit_block_stmt(n.try_block());
        if let Some(catch) = n.catch_clause() {
            self.emitter.print().p_whitespace();
            self.emit_catch_block(catch);
        }
        if let Some(finally) = n.finally_block() {
            self.emitter.print().p("finally");
            self.emitter.print().p_whitespace();
            self.emit_block_stmt(finally);
        }
    }

    fn emit_continue_stmt(&mut self, n: ir::ContinueStmtID) {
        let n = self.nodes.get_continue_stmt(&n);
        self.emitter.print().p("continue");
        if let Some(label) = n.label() {
            self.emitter.print().p_whitespace();
            self.emit_ident(label);
        }
        self.emitter.print().p_semi();
    }

    fn emit_break_stmt(&mut self, n: ir::BreakStmtID) {
        let n = self.nodes.get_break_stmt(&n);
        self.emitter.print().p("break");
        if let Some(label) = n.label() {
            self.emitter.print().p_whitespace();
            self.emit_ident(label);
        }
        self.emitter.print().p_semi();
    }

    fn emit_for_in_stmt(&mut self, n: ir::ForInStmtID) {
        let n = self.nodes.get_for_in_stmt(&n);
        self.emitter.print().p("for");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("(");
        self.emitter.print().p_whitespace();
        self.emit_for_init(n.init());
        self.emitter.print().p_whitespace();
        self.emitter.print().p("in");
        self.emitter.print().p_whitespace();
        self.emit_expr(n.expr());
        self.emitter.print().p(")");
        self.emitter.print().p_whitespace();
        self.emit_stmt(n.body());
    }

    fn emit_for_stmt(&mut self, n: ir::ForStmtID) {
        let n = self.nodes.get_for_stmt(&n);

        self.emitter.print().p("for");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("(");
        self.emitter.print().p_whitespace();
        if let Some(init) = n.init() {
            self.emit_for_init(init);
        }
        self.emitter.print().p_semi();
        self.emitter.print().p_whitespace();
        if let Some(cond) = n.cond() {
            self.emit_expr(cond);
        }
        self.emitter.print().p_semi();
        self.emitter.print().p_whitespace();
        if let Some(incr) = n.incr() {
            self.emit_expr(incr);
        }
        self.emitter.print().p(")");
        self.emitter.print().p_whitespace();
        self.emit_stmt(n.body());
    }

    fn emit_for_init(&mut self, n: &'ir ir::ForInit) {
        match n {
            ir::ForInit::Var(decls) => {
                self.emitter.print().p("var");
                self.emitter.print().p_whitespace();
                self.emit_var_decls(decls);
            }
            ir::ForInit::Expr(expr) => self.emit_expr(*expr),
        }
    }

    fn emit_for_of_stmt(&mut self, n: ir::ForOfStmtID) {
        let n = self.nodes.get_for_of_stmt(&n);

        self.emitter.print().p("for");
        self.emitter.print().p_whitespace();
        if n.r#await().is_some() {
            self.emitter.print().p("await");
            self.emitter.print().p_whitespace();
        }
        self.emitter.print().p("(");
        self.emitter.print().p_whitespace();
        self.emit_for_init(n.init());
        self.emitter.print().p_whitespace();
        self.emitter.print().p("of");
        self.emitter.print().p_whitespace();
        self.emit_expr(n.expr());
        self.emitter.print().p(")");
        self.emitter.print().p_whitespace();
        self.emit_stmt(n.body());
    }

    fn emit_export_spec(&mut self, spec: ir::ExportSpec) {
        match spec {
            ir::ExportSpec::Shorthand(n) => self.emit_shorthand_spec(n),
            ir::ExportSpec::Named(n) => self.emit_export_named_spec(n),
        }
    }

    fn emit_export_named_spec(&mut self, n: ir::ExportNamedSpecID) {
        let n = self.nodes.get_export_named_spec(&n);
        self.emit_module_export_name(n.prop_name());
        self.emitter.print().p_whitespace();
        self.emitter.print().p("as");
        self.emitter.print().p_whitespace();
        self.emit_module_export_name(n.name());
    }

    fn emit_export_decl(&mut self, n: ir::ExportDeclID) {
        let n = self.nodes.get_export_decl(&n);

        self.emitter.print().p("export");
        self.emitter.print().p_whitespace();
        match n.clause() {
            ir::ExportClause::Specs(specs) => {
                self.emitter.print().p("{");
                self.emitter.print().p_whitespace();
                let specs = self.nodes.get_specs_export(&specs);
                self.emit_list(
                    specs.list(),
                    |this, spec| this.emit_export_spec(*spec),
                    |this, _| {
                        this.emitter.content.p_comma();
                        this.emitter.content.p_whitespace();
                    },
                );
                self.emitter.print().p_whitespace();
                self.emitter.print().p("}");
                if let Some(module) = specs.module() {
                    self.emitter.print().p_whitespace();
                    self.emitter.print().p("from");
                    self.emitter.print().p_whitespace();
                    self.emit_string_lit(module);
                }
            }
            ir::ExportClause::Ns(n) => self.emit_ns_export(n),
            ir::ExportClause::Glob(n) => {
                self.emitter.print().p("*");
                self.emitter.print().p_whitespace();
                self.emitter.print().p("from");
                self.emitter.print().p_whitespace();
                self.emit_string_lit(self.nodes.get_glob_export(&n).name());
            }
        }
    }

    fn emit_ns_export(&mut self, ns: ir::NsExportID) {
        let ns = self.nodes.get_ns_export(&ns);
        self.emitter.print().p("*");
        self.emitter.print().p_whitespace();
        self.emitter.print().p("as");
        self.emitter.print().p_whitespace();
        self.emit_module_export_name(ns.name());
        self.emitter.print().p_whitespace();
        self.emitter.print().p("from");
        self.emitter.print().p_whitespace();
        self.emit_string_lit(ns.module());
    }

    fn emit_assign_expr(&mut self, n: ir::AssignExprID) {
        let n = self.nodes.get_assign_expr(&n);
        self.emit_expr(n.left());
        self.emitter.print().p_whitespace();
        self.emitter.print().p(n.op().as_str());
        self.emitter.print().p_whitespace();
        self.emit_expr(n.right());
    }

    fn emit_bin_expr(&mut self, n: ir::BinExprID) {
        let n = self.nodes.get_bin_expr(&n);
        self.emit_expr(n.left());
        self.emitter.print().p_whitespace();
        self.emitter.print().p(n.op().kind.as_str());
        self.emitter.print().p_whitespace();
        self.emit_expr(n.right());
    }

    fn emit_paren_expr(&mut self, n: ir::ParenExprID) {
        let n = self.nodes.get_paren_expr(&n);
        self.emitter.print().p_l_paren();
        self.emit_expr(n.expr());
        self.emitter.print().p_r_paren();
    }

    fn emit_this_expr(&mut self, _: ir::ThisExprID) {
        self.emitter.print().p("this");
    }

    fn emit_bigint_lit(&mut self, n: ir::BigIntLitID) {
        let n = self.nodes.get_bigint_lit(&n);
        if n.val().0 {
            self.emitter.print().p("-");
        }
        let content = self.emitter.atoms.get(n.val().1);
        self.emitter.print().p(content);
        self.emitter.print().p("n");
    }

    fn emit_regexp_lit(&mut self, n: ir::RegExpLitID) {
        let n = self.nodes.get_regexp_lit(&n);
        let content = self.emitter.atoms.get(n.val());
        self.emitter.print().p(content);
    }

    fn emit_array_lit(&mut self, n: ir::ArrayLitID) {
        let n = self.nodes.get_array_lit(&n);
        self.emitter.print().p_l_bracket();
        for (idx, expr) in n.elems().iter().enumerate() {
            self.emit_expr(*expr);
            if idx != n.elems().len() - 1 {
                self.emitter.print().p_comma();
                self.emitter.print().p_whitespace();
            }
        }
        self.emitter.print().p_r_bracket();
    }

    fn emit_object_lit(&mut self, n: ir::ObjectLitID) {
        let n = self.nodes.get_object_lit(&n);
        if n.members().is_empty() {
            self.emitter.print().p("{}");
            return;
        }
        self.emitter.print().p_l_brace();
        self.emitter.print().p_newline();
        self.emitter.print().indent += self.emitter.options.indent;
        let indent = self.emitter.print().indent;
        self.emitter.print().p_pieces_of_whitespace(indent);
        self.emit_list(
            n.members(),
            |this, member| {
                this.emit_object_member(*member);
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

    fn emit_object_member(&mut self, field: ir::ObjectLitMember) {
        match field {
            ir::ObjectLitMember::Prop(n) => self.emit_object_prop_member(n),
            ir::ObjectLitMember::Shorthand(n) => self.emit_object_shorthand_member(n),
            ir::ObjectLitMember::Method(n) => self.emit_object_method_member(n),
            ir::ObjectLitMember::SpreadAssignment(n) => {
                self.emitter.print().p("...");
                let n = self.nodes.get_spread_assignment(&n);
                self.emit_expr(n.expr());
            }
            ir::ObjectLitMember::Getter(n) => {
                self.emit_getter_decl(n);
            }
            ir::ObjectLitMember::Setter(n) => {
                self.emit_setter_decl(n);
            }
        }
    }

    fn emit_object_method_member(&mut self, method: ir::ObjectMethodMemberID) {
        let n = self.nodes.get_object_method_member(&method);
        self.emit_prop_name(n.name());
        self.emit_params(n.params());
        self.emitter.print().p_whitespace();
        self.emit_block_stmt(n.body());
    }

    fn emit_object_prop_member(&mut self, prop: ir::ObjectPropMemberID) {
        let n = self.nodes.get_object_prop_member(&prop);
        self.emit_prop_name(n.name());
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.emit_expr(n.init());
    }

    fn emit_object_shorthand_member(&mut self, shorthand: ir::ObjectShorthandMemberID) {
        let n = self.nodes.get_object_shorthand_member(&shorthand);
        self.emit_ident(n.name());
    }

    fn emit_prop_access_expr(&mut self, n: ir::PropAccessExprID) {
        let n = self.nodes.get_prop_access_expr(&n);
        if let ir::Expr::NumLit(id) = n.expr() {
            self.emit_num_lit(id);
            if self.nodes.get_num_lit(&id).val().fract() == 0. {
                self.emitter.print().p(".");
            }
        } else {
            self.emit_expr(n.expr());
        }
        self.emitter.print().p_dot();
        self.emit_ident(n.name());
    }

    fn emit_template_expr(&mut self, n: ir::TemplateExprID) {
        let n = self.nodes.get_template_expr(&n);
        self.emitter.print().p("`");
        let head = self.nodes.get_template_head(&n.head());
        let content = self.emitter.atoms.get(head.text());
        self.emitter.print().p(content);
        for span in n.spans() {
            self.emitter.print().p("${");
            let span = self.nodes.get_template_span(span);
            self.emit_expr(span.expr());
            self.emitter.print().p("}");
            let content = self.emitter.atoms.get(span.text());
            self.emitter.print().p(content);
        }
        self.emitter.print().p("`");
    }

    fn emit_expr(&mut self, expr: ir::Expr) {
        match expr {
            ir::Expr::Assign(id) => self.emit_assign_expr(id),
            ir::Expr::Bin(id) => self.emit_bin_expr(id),
            ir::Expr::Omit(_) => {}
            ir::Expr::Paren(id) => self.emit_paren_expr(id),
            ir::Expr::This(id) => self.emit_this_expr(id),
            ir::Expr::Ident(id) => self.emit_ident(id),
            ir::Expr::BoolLit(id) => {
                let n = self.nodes.get_bool_lit(&id);
                self.emitter.print().p(&n.val().to_string());
            }
            ir::Expr::NullLit(_) => {
                self.emitter.print().p("null");
            }
            ir::Expr::NumLit(id) => self.emit_num_lit(id),
            ir::Expr::BigIntLit(id) => self.emit_bigint_lit(id),
            ir::Expr::RegExpLit(id) => self.emit_regexp_lit(id),
            ir::Expr::StringLit(id) => self.emit_string_lit(id),
            ir::Expr::ArrayLit(id) => self.emit_array_lit(id),
            ir::Expr::ObjectLit(id) => self.emit_object_lit(id),
            ir::Expr::Void(id) => {
                let n = self.nodes.get_void_expr(&id);
                self.emitter.print().p("void");
                self.emitter.print().p_whitespace();
                self.emit_expr(n.expr());
            }
            ir::Expr::Typeof(id) => {
                let n = self.nodes.get_typeof_expr(&id);
                self.emitter.print().p("typeof");
                self.emitter.print().p_whitespace();
                self.emit_expr(n.expr());
            }
            ir::Expr::Super(_) => {
                self.emitter.print().p("super");
            }
            ir::Expr::EleAccess(id) => {
                let n = self.nodes.get_ele_access_expr(&id);
                self.emit_expr(n.expr());
                self.emitter.print().p_l_bracket();
                self.emit_expr(n.arg());
                self.emitter.print().p_r_bracket();
            }
            ir::Expr::PropAccess(id) => self.emit_prop_access_expr(id),
            ir::Expr::PostfixUnary(id) => {
                let n = self.nodes.get_postfix_unary_expr(&id);
                self.emit_expr(n.expr());
                self.emitter.print().p(n.op().as_str());
            }
            ir::Expr::PrefixUnary(id) => {
                let n = self.nodes.get_prefix_unary_expr(&id);
                self.emitter.print().p(n.op().as_str());
                if let ir::Expr::PrefixUnary(_) = n.expr() {
                    self.emitter.print().p_whitespace();
                }
                self.emit_expr(n.expr());
            }
            ir::Expr::TaggedTemplate(id) => {
                let n = self.nodes.get_tagged_template_expr(&id);
                self.emit_expr(n.tag());
                self.emit_expr(n.tpl());
            }
            ir::Expr::Template(id) => self.emit_template_expr(id),
            ir::Expr::SpreadElem(id) => {
                let n = self.nodes.get_spread_element(&id);
                self.emitter.print().p("...");
                self.emit_expr(n.expr());
            }
            ir::Expr::ArrowFn(id) => self.emit_arrow_fn(id),
            ir::Expr::New(id) => self.emit_new_expr(id),
            ir::Expr::Class(id) => self.emit_class_expr(id),
            ir::Expr::Fn(id) => self.emit_fn_expr(id),
            ir::Expr::Call(id) => self.emit_call_expr(id),
            ir::Expr::Cond(id) => self.emit_cond_expr(id),
            ir::Expr::JsxElem(id) => self.emit_jsx_elem(id),
            ir::Expr::JsxSelfClosingElem(id) => self.emit_jsx_self_closing_ele(id),
            ir::Expr::JsxFrag(id) => self.emit_jsx_frag(id),
            ir::Expr::Delete(id) => {
                self.emitter.print().p("delete");
                self.emitter.print().p_whitespace();
                self.emit_expr(self.nodes.get_delete_expr(&id).expr());
            }
            ir::Expr::Await(id) => {
                self.emitter.print().p("await");
                self.emitter.print().p_whitespace();
                self.emit_expr(self.nodes.get_await_expr(&id).expr());
            }
        }
    }

    fn emit_jsx_ns_name(&mut self, n: ir::JsxNsNameID) {
        let n = self.nodes.get_jsx_ns_name(&n);
        self.emit_ident(n.ns());
        self.emitter.print().p(":");
        self.emit_ident(n.name());
    }

    fn emit_jsx_tag_name(&mut self, n: ir::JsxTagName) {
        match n {
            ir::JsxTagName::Ident(ident) => self.emit_ident(ident),
            ir::JsxTagName::Ns(n) => self.emit_jsx_ns_name(n),
            ir::JsxTagName::PropAccess(n) => self.emit_prop_access_expr(n),
            ir::JsxTagName::This(_) => self.emitter.print().p("this"),
        };
    }

    fn emit_jsx_attrs(&mut self, attrs: &[ir::JsxAttr]) {
        for attr in attrs {
            self.emitter.print().p_whitespace();
            self.emit_jsx_attr(*attr);
        }
    }

    fn emit_jsx_attr(&mut self, n: ir::JsxAttr) {
        match n {
            ir::JsxAttr::Spread(n) => {
                self.emitter.print().p_l_brace();
                self.emitter.print().p_dot_dot_dot();
                let n = self.nodes.get_jsx_spread_attr(&n);
                self.emit_expr(n.expr());
                self.emitter.print().p_r_brace();
            }
            ir::JsxAttr::Named(n) => {
                let n = self.nodes.get_jsx_named_attr(&n);
                match n.name() {
                    ir::JsxAttrName::Ident(n) => self.emit_ident(n),
                    ir::JsxAttrName::Ns(ns) => self.emit_jsx_ns_name(ns),
                };
                self.emitter.print().p_eq();
                if let Some(v) = n.init() {
                    self.emit_jsx_attr_value(v);
                }
            }
        };
    }

    fn emit_jsx_expr(&mut self, n: ir::JsxExprID) {
        let n = self.nodes.get_jsx_expr(&n);
        self.emitter.print().p_l_brace();
        if n.dotdotdot().is_some() {
            self.emitter.print().p_dot_dot_dot();
        }
        if let Some(expr) = n.expr() {
            self.emit_expr(expr);
        }
        self.emitter.print().p_r_brace();
    }

    fn emit_jsx_child(&mut self, child: ir::JsxChild) {
        match child {
            ir::JsxChild::Text(n) => {
                let n = self.nodes.get_jsx_text(&n);
                let content = self.emitter.atoms.get(n.text());
                self.emitter.print().p(content);
            }
            ir::JsxChild::Expr(n) => self.emit_jsx_expr(n),
            ir::JsxChild::Elem(n) => self.emit_jsx_elem(n),
            ir::JsxChild::SelfClosingEle(n) => {
                self.emit_jsx_self_closing_ele(n);
            }
            ir::JsxChild::Frag(n) => self.emit_jsx_frag(n),
        }
    }

    fn emit_jsx_children(&mut self, children: &[ir::JsxChild]) {
        for child in children {
            self.emit_jsx_child(*child);
        }
    }

    fn emit_jsx_attr_value(&mut self, n: ir::JsxAttrValue) {
        match n {
            ir::JsxAttrValue::StringLit(id) => self.emit_string_lit(id),
            ir::JsxAttrValue::Expr(id) => self.emit_jsx_expr(id),
            ir::JsxAttrValue::Ele(id) => self.emit_jsx_elem(id),
            ir::JsxAttrValue::SelfClosingEle(id) => self.emit_jsx_self_closing_ele(id),
            ir::JsxAttrValue::Frag(id) => self.emit_jsx_frag(id),
        };
    }

    fn emit_jsx_frag(&mut self, n: ir::JsxFragID) {
        let n = self.nodes.get_jsx_frag(&n);
        self.emitter.print().p("<>");
        self.emit_jsx_children(n.children());
        self.emitter.print().p("</>");
    }

    fn emit_jsx_self_closing_ele(&mut self, n: ir::JsxSelfClosingElemID) {
        let n = self.nodes.get_jsx_self_closing_elem(&n);
        self.emitter.print().p("<");
        self.emit_jsx_tag_name(n.tag_name());
        self.emitter.print().p_whitespace();
        self.emit_jsx_attrs(n.attrs());
        self.emitter.print().p(" />");
    }

    fn emit_jsx_elem(&mut self, elem: ir::JsxElemID) {
        let n = self.nodes.get_jsx_elem(&elem);
        self.emitter.print().p("<");
        self.emit_jsx_tag_name(
            self.nodes
                .get_jsx_opening_elem(&n.opening_elem())
                .tag_name(),
        );
        self.emitter.print().p_whitespace();
        self.emit_jsx_attrs(self.nodes.get_jsx_opening_elem(&n.opening_elem()).attrs());
        self.emitter.print().p(">");

        self.emit_jsx_children(n.children());

        self.emitter.print().p("</");
        self.emit_jsx_tag_name(
            self.nodes
                .get_jsx_closing_elem(&n.closing_elem())
                .tag_name(),
        );
        self.emitter.print().p(">");
    }

    fn emit_cond_expr(&mut self, cond: ir::CondExprID) {
        let n = self.nodes.get_cond_expr(&cond);
        self.emit_expr(n.cond());
        self.emitter.print().p_whitespace();
        self.emitter.print().p_question();
        self.emitter.print().p_whitespace();
        self.emit_expr(n.when_true());
        self.emitter.print().p_whitespace();
        self.emitter.print().p_colon();
        self.emitter.print().p_whitespace();
        self.emit_expr(n.when_false());
    }

    fn emit_fn_expr(&mut self, f: ir::FnExprID) {
        let n = self.nodes.get_fn_expr(&f);
        self.emitter.print().p("function");
        self.emitter.print().p_whitespace();
        if let Some(name) = n.name() {
            self.emit_ident(name);
        }
        self.emit_params(n.params());
        self.emitter.print().p_whitespace();

        self.emit_basic_block_with_brace(n.body(), ir::BasicBlockID::ENTRY);
    }

    fn emit_call_expr(&mut self, call: ir::CallExprID) {
        let n = self.nodes.get_call_expr(&call);
        self.emit_expr(n.callee());
        self.emit_args(n.args());
    }

    fn emit_class_expr(&mut self, n: ir::ClassExprID) {
        let n = self.nodes.get_class_expr(&n);
        self.emitter.print().p("class");
        self.emitter.print().p_whitespace();
        if let Some(ident) = n.name() {
            self.emit_ident(ident);
            self.emitter.print().p_whitespace();
            let name = self.nodes.get_ident(&ident).name();
            self.ns_names.insert((self.scope, name));
        }
        if let Some(extends) = n.extends() {
            self.emit_class_extends_clause(extends);
        }
        self.emitter.print().p_l_brace();
        if !n.elems().is_empty() {
            self.emitter.print().indent += self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emit_list(
            n.elems(),
            |this, elem| {
                this.emit_class_elem(*elem);
            },
            |this, _| {
                this.emitter.content.p_newline();
            },
        );
        if !n.elems().is_empty() {
            self.emitter.print().indent -= self.emitter.options.indent;
            self.emitter.print().p_newline();
        }
        self.emitter.print().p_r_brace();
    }

    fn emit_args(&mut self, args: &[ir::Expr]) {
        self.emitter.print().p_l_paren();
        self.emit_list(
            args,
            |this, arg| this.emit_expr(*arg),
            |this, _| {
                this.emitter.content.p_comma();
                this.emitter.content.p_whitespace();
            },
        );
        self.emitter.print().p_r_paren();
    }

    fn emit_new_expr(&mut self, n: ir::NewExprID) {
        let n = self.nodes.get_new_expr(&n);
        self.emitter.print().p("new");
        self.emitter.print().p_whitespace();
        self.emit_expr(n.expr());
        self.emit_args(n.args());
    }

    fn emit_arrow_fn(&mut self, f: ir::ArrowFnExprID) {
        let n = self.nodes.get_arrow_fn_expr(&f);
        self.emit_params(n.params());
        self.emitter.print().p_whitespace();
        self.emitter.print().p("=>");
        self.emitter.print().p_whitespace();

        let graph = self.graph(n.body());
        let block = graph.get_basic_block(ir::BasicBlockID::ENTRY);
        if block.stmts().len() == 1
            && let ir::Stmt::Ret(ret) = block.stmts()[0]
            && let Some(expr) = self.nodes.get_ret_stmt(&ret).expr()
        {
            self.emitter.print().p("(");
            self.emit_expr(expr);
            self.emitter.print().p(")");
        } else {
            self.emit_basic_block_with_brace(n.body(), ir::BasicBlockID::ENTRY);
        }
    }
}
