use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr};

use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_checker::check::TyChecker;
use bolt_ts_ecma_logical::js_double_to_int32;
use bolt_ts_span::{ModuleID, Span};

use crate::ir;

struct LoweringCtx<'checker, 'cx> {
    checker: &'checker mut TyChecker<'cx>,
    nodes: ir::Nodes,
    graph_arena: ir::GraphArena,
    current: (ir::GraphID, ir::BasicBlockID),
}

pub struct LoweringResult {
    pub entry_graph: ir::GraphID,

    pub nodes: ir::Nodes,
    pub graph_arena: ir::GraphArena,
}

pub(super) fn lowering<'cx>(item: ModuleID, checker: &mut TyChecker<'cx>) -> LoweringResult {
    let mut ctx = LoweringCtx::new(checker);
    let (entry_graph, _) = ctx.current;
    let root = ctx.checker.p.root(item);
    ctx.lower_program(root);
    LoweringResult {
        entry_graph,
        graph_arena: ctx.graph_arena,
        nodes: ctx.nodes,
    }
}

impl<'checker, 'cx> LoweringCtx<'checker, 'cx> {
    fn new(checker: &'checker mut TyChecker<'cx>) -> Self {
        let mut graph_arena = ir::GraphArena::default();
        let current_graph = graph_arena.alloc_empty_graph();
        let current_basic_block = graph_arena.get_mut(current_graph).alloc_empty_basic_block();
        Self {
            checker,
            graph_arena,
            current: (current_graph, current_basic_block),
            nodes: ir::Nodes::default(),
        }
    }

    fn add_stmt_to_basic_block(&mut self, stmt: ir::Stmt, loc: (ir::GraphID, ir::BasicBlockID)) {
        let (g, bb) = loc;
        let bb = self.graph_arena.get_mut(g).get_mut_basic_block(bb);
        bb.add_stmt(stmt);
    }

    fn lower_program(&mut self, root: &'cx ast::Program<'cx>) {
        debug_assert!(self.current.1 == ir::BasicBlockID::ENTRY);
        let saved = self.current;
        for stmt in self.lower_stmts(root.stmts) {
            self.add_stmt_to_basic_block(stmt, saved);
        }
        self.current = saved;
    }

    fn lower_stmts(&mut self, stmts: ast::Stmts<'cx>) -> Vec<ir::Stmt> {
        stmts
            .iter()
            .filter_map(|stmt| self.lower_stmt(stmt))
            .collect()
    }

    fn lower_stmt(&mut self, stmt: &'cx ast::Stmt<'cx>) -> Option<ir::Stmt> {
        use ast::StmtKind;
        use ir::Stmt;
        match stmt.kind {
            StmtKind::Var(n) => Some(Stmt::Var(self.lower_var_stmt(n))),
            StmtKind::If(n) => Some(Stmt::If(self.lower_if_stmt(n))),
            StmtKind::For(n) => Some(Stmt::For(self.lower_for_stmt(n))),
            StmtKind::ForOf(n) => Some(Stmt::ForOf(self.lower_for_of_stmt(n))),
            StmtKind::ForIn(n) => Some(Stmt::ForIn(self.lower_for_in_stmt(n))),
            StmtKind::Break(n) => Some(Stmt::Break(self.lower_break_stmt(n))),
            StmtKind::Continue(n) => Some(Stmt::Continue(self.lower_continue_stmt(n))),
            StmtKind::Ret(n) => Some(Stmt::Ret(self.lower_ret_stmt(n))),
            StmtKind::Block(n) => Some(Stmt::Block(self.lower_block_stmt(n))),
            StmtKind::Fn(n) => self.lower_fn_decl(n).map(Stmt::Fn),
            StmtKind::Class(n) => self.lower_class_decl(n).map(Stmt::Class),
            StmtKind::Expr(n) => Some(Stmt::Expr({
                let expr = self.lower_expr(n.expr);
                self.nodes.alloc_expr_stmt(n.span, expr)
            })),
            StmtKind::Module(n) => self.lower_module_decl(n).map(Stmt::Module),
            StmtKind::Throw(n) => Some(Stmt::Throw(self.lower_throw_stmt(n))),
            StmtKind::Enum(n) => Some(Stmt::Enum(self.lower_enum_decl(n))),
            StmtKind::Import(n) => Some(Stmt::Import(self.lower_import_decl(n))),
            StmtKind::Export(n) => Some(Stmt::Export(self.lower_export_stmt(n))),
            StmtKind::ExportAssign(n) => Some(Stmt::ExportAssign(self.lower_export_assign(n))),
            StmtKind::Try(n) => Some(Stmt::Try(self.lower_try_stmt(n))),
            StmtKind::While(n) => Some(Stmt::While(self.lower_while_stmt(n))),
            StmtKind::Do(n) => Some(Stmt::Do(self.lower_do_stmt(n))),
            StmtKind::Labeled(n) => Some(Stmt::Labeled(self.lower_labeled_stmt(n))),
            StmtKind::Switch(n) => Some(Stmt::Switch(self.lower_switch_stmt(n))),
            StmtKind::Empty(n) => Some(Stmt::Empty(self.nodes.alloc_empty_stmt(n.span))),
            StmtKind::TypeAlias(_) | StmtKind::Interface(_) | StmtKind::Debugger(_) => None,
        }
    }

    fn lower_switch_stmt(&mut self, n: &'cx ast::SwitchStmt<'cx>) -> ir::SwitchStmtID {
        let expr = self.lower_expr(n.expr);
        let case_block = self.lower_case_block(n.case_block);
        self.nodes.alloc_switch_stmt(n.span, expr, case_block)
    }

    fn lower_case_block(&mut self, n: &'cx ast::CaseBlock<'cx>) -> ir::CaseBlockID {
        let clauses = n
            .clauses
            .iter()
            .map(|clause| match clause {
                ast::CaseOrDefaultClause::Case(n) => {
                    ir::CaseOrDefaultClause::Case(self.lower_case_clause(n))
                }
                ast::CaseOrDefaultClause::Default(n) => {
                    ir::CaseOrDefaultClause::Default(self.lower_default_clause(n))
                }
            })
            .collect();
        self.nodes.alloc_case_block(n.span, clauses)
    }

    fn lower_default_clause(&mut self, n: &'cx ast::DefaultClause<'cx>) -> ir::DefaultClauseID {
        let stmts = self.lower_stmts(n.stmts);
        self.nodes.alloc_default_clause(n.span, stmts)
    }

    fn lower_case_clause(&mut self, n: &'cx ast::CaseClause<'cx>) -> ir::CaseClauseID {
        let expr = self.lower_expr(n.expr);
        let stmts = self.lower_stmts(n.stmts);
        self.nodes.alloc_case_clause(n.span, expr, stmts)
    }

    fn lower_labeled_stmt(&mut self, n: &'cx ast::LabeledStmt<'cx>) -> ir::LabeledStmtID {
        let label = self.lower_ident(n.label);
        let stmt = self.lower_stmt(n.stmt).unwrap();
        self.nodes.alloc_labeled_stmt(n.span, label, stmt)
    }

    fn lower_do_stmt(&mut self, n: &'cx ast::DoWhileStmt<'cx>) -> ir::DoStmtID {
        let stmt = self.lower_stmt(n.stmt).unwrap();
        let expr = self.lower_expr(n.expr);
        self.nodes.alloc_do_stmt(n.span, stmt, expr)
    }

    fn lower_while_stmt(&mut self, n: &'cx ast::WhileStmt<'cx>) -> ir::WhileStmtID {
        let expr = self.lower_expr(n.expr);
        let body = self.lower_stmt(n.stmt).unwrap();
        self.nodes.alloc_while_stmt(n.span, expr, body)
    }

    fn lower_try_stmt(&mut self, n: &'cx ast::TryStmt<'cx>) -> ir::TryStmtID {
        let try_block = self.lower_block_stmt(n.try_block);
        let catch = n.catch_clause.map(|c| self.lower_catch_clause(c));
        let finally = n.finally_block.map(|f| self.lower_block_stmt(f));
        self.nodes.alloc_try_stmt(n.span, try_block, catch, finally)
    }

    fn lower_catch_clause(&mut self, n: &'cx ast::CatchClause<'cx>) -> ir::CatchClauseID {
        let var = n.var.map(|b| self.lower_var_decl(b));
        let block = self.lower_block_stmt(n.block);
        self.nodes.alloc_catch_clause(n.span, var, block)
    }

    fn lower_export_assign(&mut self, n: &'cx ast::ExportAssign<'cx>) -> ir::ExportAssignID {
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let expr = self.lower_expr(n.expr);
        self.nodes.alloc_export_assign(n.span, modifiers, expr)
    }

    fn lower_export_stmt(&mut self, n: &'cx ast::ExportDecl<'cx>) -> ir::ExportDeclID {
        let clause = match n.clause.kind {
            ast::ExportClauseKind::Glob(n) => {
                let name = self.lower_string_lit(n.module);
                ir::ExportClause::Glob(self.nodes.alloc_glob_export(n.span, name))
            }
            ast::ExportClauseKind::Ns(n) => {
                let name = self.lower_module_export_name(n.name);
                let module = self.lower_string_lit(n.module);
                ir::ExportClause::Ns(self.nodes.alloc_ns_export(n.span, name, module))
            }
            ast::ExportClauseKind::Specs(ns) => {
                let specs = ns
                    .list
                    .iter()
                    .map(|n| match n.kind {
                        ast::ExportSpecKind::Shorthand(n) => {
                            let name = self.lower_ident(n.name);
                            ir::ExportSpec::Shorthand(self.nodes.alloc_shorthand_spec(n.span, name))
                        }
                        ast::ExportSpecKind::Named(n) => {
                            let prop_name = self.lower_module_export_name(n.prop_name);
                            let name = self.lower_module_export_name(n.name);
                            ir::ExportSpec::Named(
                                self.nodes.alloc_export_named_spec(n.span, prop_name, name),
                            )
                        }
                    })
                    .collect();
                let module = ns.module.map(|m| self.lower_string_lit(m));
                ir::ExportClause::Specs(self.nodes.alloc_specs_export(n.span, specs, module))
            }
        };
        self.nodes.alloc_export_decl(n.span, clause)
    }

    fn lower_import_decl(&mut self, n: &'cx ast::ImportDecl<'cx>) -> ir::ImportDeclID {
        let clause = n.clause.map(|c| self.lower_import_clause(c));
        let module = self.lower_string_lit(n.module);
        self.nodes.alloc_import_decl(n.span, clause, module)
    }

    fn lower_import_clause(&mut self, n: &'cx ast::ImportClause<'cx>) -> ir::ImportClauseID {
        let name = n.name.map(|n| self.lower_ident(n));
        let kind = n.kind.map(|k| match k {
            ast::ImportClauseKind::Ns(n) => {
                let ident = self.lower_ident(n.name);
                ir::ImportClauseKind::Ns(self.nodes.alloc_ns_import(n.span, ident))
            }
            ast::ImportClauseKind::Specs(ns) => ir::ImportClauseKind::Specs(
                ns.iter()
                    .map(|n| match n.kind {
                        ast::ImportSpecKind::Shorthand(n) => {
                            let name = self.lower_ident(n.name);
                            ir::ImportSpec::Shorthand(self.nodes.alloc_shorthand_spec(n.span, name))
                        }
                        ast::ImportSpecKind::Named(n) => {
                            let prop_name = self.lower_module_export_name(n.prop_name);
                            let name = self.lower_ident(n.name);
                            let id = self.nodes.alloc_import_named_spec(n.span, prop_name, name);
                            ir::ImportSpec::Named(id)
                        }
                    })
                    .collect(),
            ),
        });
        self.nodes.alloc_import_clause(n.span, name, kind)
    }

    fn lower_module_export_name(
        &mut self,
        n: &'cx ast::ModuleExportName<'cx>,
    ) -> ir::ModuleExportName {
        match n.kind {
            ast::ModuleExportNameKind::Ident(n) => ir::ModuleExportName::Ident(self.lower_ident(n)),
            ast::ModuleExportNameKind::StringLit(n) => {
                ir::ModuleExportName::StringLit(self.lower_string_lit(n))
            }
        }
    }

    fn lower_enum_decl(&mut self, n: &'cx ast::EnumDecl<'cx>) -> ir::EnumDeclID {
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let name = self.lower_ident(n.name);
        let members = n
            .members
            .iter()
            .map(|m| self.lower_enum_member(m))
            .collect();
        self.nodes.alloc_enum_decl(n.span, modifiers, name, members)
    }

    fn lower_enum_member(&mut self, n: &'cx ast::EnumMember<'cx>) -> ir::EnumMemberID {
        let name = self.lower_prop_name(n.name);
        let init = n.init.map(|init| self.lower_expr(init));
        self.nodes.alloc_enum_member(n.span, name, init)
    }

    fn lower_throw_stmt(&mut self, n: &'cx ast::ThrowStmt<'cx>) -> ir::ThrowStmtID {
        let expr = self.lower_expr(n.expr);
        self.nodes.alloc_throw_stmt(n.span, expr)
    }

    fn lower_module_decl(&mut self, n: &'cx ast::ModuleDecl<'cx>) -> Option<ir::ModuleDeclID> {
        let block = n.block?;
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let name = match n.name {
            ast::ModuleName::Ident(name) => ir::ModuleName::Ident(self.lower_ident(name)),
            ast::ModuleName::StringLit(name) => {
                ir::ModuleName::StringLit(self.lower_string_lit(name))
            }
        };
        let stmts = self.lower_stmts(block.stmts);
        let block = self.nodes.alloc_module_block(block.span, stmts);
        Some(self.nodes.alloc_module_decl(n.span, modifiers, name, block))
    }

    fn lower_string_lit(&mut self, n: &'cx ast::StringLit) -> ir::StringLitID {
        self.nodes.alloc_string_lit(n.span, n.val, false)
    }

    fn lower_class_decl(&mut self, n: &'cx ast::ClassDecl<'cx>) -> Option<ir::ClassDeclID> {
        if n.modifiers
            .is_some_and(|m| m.flags.contains(ast::ModifierKind::Ambient))
        {
            return None;
        }
        let Some(name) = n.name else { todo!() };
        let name = self.lower_ident(name);
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let extends = n.extends.map(|e| self.lower_class_extends_clause(e));
        let elems = self.lower_class_elems(n.elems);
        let n = self
            .nodes
            .alloc_class_decl(n.span, modifiers, name, extends, elems);
        Some(n)
    }

    fn lower_class_extends_clause(
        &mut self,
        extends: &'cx ast::ClassExtendsClause<'cx>,
    ) -> ir::ClassExtendsClauseID {
        let expr = self.lower_expr(extends.expr_with_ty_args.expr);
        self.nodes.alloc_class_extends_clause(extends.span, expr)
    }

    fn lower_class_elems(&mut self, elems: &'cx ast::ClassElems<'cx>) -> Vec<ir::ClassElem> {
        elems
            .list
            .iter()
            .filter_map(|elem| self.lower_class_elem(elem))
            .collect()
    }

    fn lower_class_elem(&mut self, elem: &'cx ast::ClassElem<'cx>) -> Option<ir::ClassElem> {
        match elem.kind {
            ast::ClassElemKind::Ctor(n) => self.lower_class_ctor(n).map(ir::ClassElem::Ctor),
            ast::ClassElemKind::Prop(n) => {
                Some(ir::ClassElem::PropElem(self.lower_class_prop_elem(n)))
            }
            ast::ClassElemKind::Method(n) => self
                .lower_class_method_elem(n)
                .map(ir::ClassElem::MethodElem),
            ast::ClassElemKind::Getter(n) => self.lower_getter_decl(n).map(ir::ClassElem::Getter),
            ast::ClassElemKind::Setter(n) => self.lower_setter_decl(n).map(ir::ClassElem::Setter),
            ast::ClassElemKind::StaticBlockDecl(n) => {
                Some(ir::ClassElem::StaticBlock(self.lower_class_static_block(n)))
            }
            ast::ClassElemKind::IndexSig(_) => None,
        }
    }

    fn lower_class_static_block(
        &mut self,
        n: &'cx ast::ClassStaticBlockDecl<'cx>,
    ) -> ir::ClassStaticBlockDeclID {
        let body = self.lower_block_stmt(n.body);
        self.nodes.alloc_class_static_block(n.span, body)
    }

    fn lower_setter_decl(&mut self, n: &'cx ast::SetterDecl<'cx>) -> Option<ir::SetterDeclID> {
        let body = n.body?;
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let name = self.lower_prop_name(n.name);
        let params = self.lower_param_decls(n.params);
        let body = self.lower_block_stmt(body);
        Some(
            self.nodes
                .alloc_setter_decl(n.span, modifiers, name, params, body),
        )
    }

    fn lower_getter_decl(&mut self, n: &'cx ast::GetterDecl<'cx>) -> Option<ir::GetterDeclID> {
        let body = n.body?;
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let name = self.lower_prop_name(n.name);
        let body = self.lower_block_stmt(body);
        Some(self.nodes.alloc_getter_decl(n.span, modifiers, name, body))
    }

    fn lower_class_method_elem(
        &mut self,
        n: &'cx ast::ClassMethodElem<'cx>,
    ) -> Option<ir::ClassMethodElemID> {
        let body = n.body?;
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let name = self.lower_prop_name(n.name);
        let params = self.lower_param_decls(n.params);
        let body = self.lower_block_stmt(body);
        Some(
            self.nodes
                .alloc_class_method_elem(n.span, modifiers, name, params, body),
        )
    }

    fn lower_class_prop_elem(&mut self, n: &'cx ast::ClassPropElem<'cx>) -> ir::ClassPropElemID {
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let name = self.lower_prop_name(n.name);
        let init = n.init.map(|init| self.lower_expr(init));
        self.nodes
            .alloc_class_prop_elem(n.span, modifiers, name, init)
    }

    fn lower_class_ctor(&mut self, n: &'cx ast::ClassCtor<'cx>) -> Option<ir::ClassCtorID> {
        let body = n.body?;
        let params = self.lower_param_decls(n.params);
        let body = self.lower_block_stmt(body);
        Some(self.nodes.alloc_class_ctor(n.span, params, body))
    }

    fn lower_fn_decl(&mut self, n: &'cx ast::FnDecl<'cx>) -> Option<ir::FnDeclID> {
        let body = n.body?;
        let modifiers = n.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let name = n.name.map(|name| self.lower_ident(name));
        let params = self.lower_param_decls(n.params);

        let saved = self.current;
        let graph = self.graph_arena.alloc_empty_graph();
        let bb = self.graph_arena.get_mut(graph).alloc_empty_basic_block();
        debug_assert!(bb == ir::BasicBlockID::ENTRY);
        self.current = (graph, bb);
        for stmt in self.lower_stmts(body.stmts) {
            self.add_stmt_to_basic_block(stmt, (graph, bb));
        }
        self.current = saved;

        let f = self
            .nodes
            .alloc_fn_decl(n.span, modifiers, name, params, graph);
        Some(f)
    }

    fn lower_param_decls(&mut self, params: ast::ParamsDecl<'cx>) -> Vec<ir::ParamDeclID> {
        params
            .iter()
            .enumerate()
            .filter_map(|(idx, param)| {
                if idx == 0 && matches!(param.name.kind, ast::BindingKind::Ident(ident) if ident.name == keyword::KW_THIS) {
                    None
                } else {
                    Some(self.lower_param_decl(param))
                }
            })
            .collect()
    }

    fn lower_param_decl(&mut self, n: &'cx ast::ParamDecl<'cx>) -> ir::ParamDeclID {
        let modifiers = n.modifiers.map(|ms| self.lower_modifiers(ms));
        let name = self.lower_binding(n.name);
        let init = n.init.map(|init| self.lower_expr(init));
        self.nodes
            .alloc_param_decl(n.span, modifiers, n.dotdotdot, name, n.question, init)
    }

    fn lower_modifiers(&mut self, modifiers: &'cx ast::Modifiers<'cx>) -> ir::Modifiers {
        let list = modifiers
            .list
            .iter()
            .map(|m| self.lower_modifier(m))
            .collect();
        ir::Modifiers::new(modifiers.flags, list)
    }

    fn lower_modifier(&mut self, n: &'cx ast::Modifier) -> ir::ModifierID {
        self.nodes.alloc_modifier(n.span, n.kind)
    }

    fn lower_block_stmt(&mut self, block: &'cx ast::BlockStmt<'cx>) -> ir::BlockStmtID {
        let stmts = self.lower_stmts(block.stmts);
        self.nodes.alloc_block_stmt(block.span, stmts)
    }

    fn lower_ret_stmt(&mut self, stmt: &'cx ast::RetStmt<'cx>) -> ir::RetStmtID {
        let expr = stmt.expr.as_ref().map(|expr| self.lower_expr(expr));
        self.nodes.alloc_ret_stmt(stmt.span, expr)
    }

    fn lower_continue_stmt(&mut self, stmt: &'cx ast::ContinueStmt<'cx>) -> ir::ContinueStmtID {
        let label = stmt.label.map(|label| self.lower_ident(label));
        self.nodes.alloc_continue_stmt(stmt.span, label)
    }

    fn lower_break_stmt(&mut self, stmt: &'cx ast::BreakStmt<'cx>) -> ir::BreakStmtID {
        let label = stmt.label.map(|label| self.lower_ident(label));
        self.nodes.alloc_break_stmt(stmt.span, label)
    }

    fn lower_for_in_stmt(&mut self, stmt: &'cx ast::ForInStmt<'cx>) -> ir::ForInStmtID {
        let init = self.lower_for_init(stmt.init);
        let expr = self.lower_expr(stmt.expr);
        let body = self.lower_stmt(stmt.body).unwrap();
        self.nodes.alloc_for_in_stmt(stmt.span, init, expr, body)
    }

    fn lower_for_of_stmt(&mut self, stmt: &'cx ast::ForOfStmt<'cx>) -> ir::ForOfStmtID {
        let init = self.lower_for_init(stmt.init);
        let expr = self.lower_expr(stmt.expr);
        let body = self.lower_stmt(stmt.body).unwrap();
        self.nodes
            .alloc_for_of_stmt(stmt.span, stmt.r#await, init, expr, body)
    }

    fn lower_for_stmt(&mut self, stmt: &'cx ast::ForStmt<'cx>) -> ir::ForStmtID {
        let init = stmt.init.map(|init| self.lower_for_init(init));
        let cond = stmt.cond.map(|cond| self.lower_expr(cond));
        let incr = stmt.incr.map(|incr| self.lower_expr(incr));
        let body = self.lower_stmt(stmt.body).unwrap();
        self.nodes.alloc_for_stmt(stmt.span, init, cond, incr, body)
    }

    fn lower_for_init(&mut self, init: ast::ForInitKind<'cx>) -> ir::ForInit {
        match init {
            ast::ForInitKind::Var(n) => ir::ForInit::Var(self.lower_var_decls(n)),
            ast::ForInitKind::Expr(n) => ir::ForInit::Expr(self.lower_expr(n)),
        }
    }

    fn lower_if_stmt(&mut self, stmt: &'cx ast::IfStmt<'cx>) -> ir::IfStmtID {
        let expr = self.lower_expr(stmt.expr);

        // then
        let then_bb = self
            .graph_arena
            .get_mut(self.current.0)
            .alloc_empty_basic_block();
        debug_assert!(then_bb != ir::BasicBlockID::ENTRY);
        let then = self.lower_stmt(stmt.then).unwrap();
        self.add_stmt_to_basic_block(then, (self.current.0, then_bb));

        // else_then
        let else_then_bb = stmt.else_then.map(|stmt| {
            let else_then_bb = self
                .graph_arena
                .get_mut(self.current.0)
                .alloc_empty_basic_block();
            debug_assert!(else_then_bb != ir::BasicBlockID::ENTRY);
            let stmt = self.lower_stmt(stmt);
            self.add_stmt_to_basic_block(stmt.unwrap(), (self.current.0, else_then_bb));
            else_then_bb
        });

        self.nodes
            .alloc_if_stmt(stmt.span, expr, then_bb, else_then_bb)
    }

    fn lower_var_stmt(&mut self, stmt: &'cx ast::VarStmt<'cx>) -> ir::VarStmtID {
        let modifiers = stmt.modifiers.as_ref().map(|ms| self.lower_modifiers(ms));
        let decls = self.lower_var_decls(stmt.list);
        self.nodes.alloc_var_stmt(stmt.span, modifiers, decls)
    }

    fn lower_var_decls(&mut self, decls: ast::VarDecls<'cx>) -> Vec<ir::VarDeclID> {
        decls.iter().map(|decl| self.lower_var_decl(decl)).collect()
    }

    fn lower_var_decl(&mut self, decl: &'cx ast::VarDecl<'cx>) -> ir::VarDeclID {
        let binding = self.lower_binding(decl.name);
        let init = decl.init.map(|init| self.lower_expr(init));
        self.nodes.alloc_var_decl(decl.span, binding, init)
    }

    fn lower_binding(&mut self, binding: &'cx ast::Binding<'cx>) -> ir::Binding {
        match binding.kind {
            ast::BindingKind::Ident(n) => ir::Binding::Ident(self.lower_ident(n)),
            ast::BindingKind::ObjectPat(n) => {
                let elems = n
                    .elems
                    .iter()
                    .map(|elem| self.lower_object_binding_elem(elem))
                    .collect();
                ir::Binding::ObjectPat(self.nodes.alloc_object_pat(n.span, elems))
            }
            ast::BindingKind::ArrayPat(n) => {
                let elems = n
                    .elems
                    .iter()
                    .map(|elem| self.lower_array_binding_ele(elem))
                    .collect();
                ir::Binding::ArrayPat(self.nodes.alloc_array_pat(n.span, elems))
            }
        }
    }

    fn lower_array_binding_ele(
        &mut self,
        elem: &'cx ast::ArrayBindingElem<'cx>,
    ) -> ir::ArrayBindingElem {
        match elem.kind {
            ast::ArrayBindingElemKind::Omit(n) => {
                ir::ArrayBindingElem::Omit(self.lower_omit_expr(n))
            }
            ast::ArrayBindingElemKind::Binding(n) => {
                ir::ArrayBindingElem::Binding(self.lower_array_binding(n))
            }
        }
    }

    fn lower_array_binding(&mut self, n: &'cx ast::ArrayBinding<'cx>) -> ir::ArrayBindingID {
        let name = self.lower_binding(n.name);
        let init = n.init.map(|init| self.lower_expr(init));
        self.nodes
            .alloc_array_binding(n.span, n.dotdotdot, name, init)
    }

    fn lower_omit_expr(&mut self, omit: &'cx ast::OmitExpr) -> ir::OmitExprID {
        self.nodes.alloc_omit_expr(omit.span)
    }

    fn lower_object_binding_elem(
        &mut self,
        elem: &'cx ast::ObjectBindingElem<'cx>,
    ) -> ir::ObjectBindingElemID {
        let name = self.lower_object_binding_name(elem.name);
        let init = elem.init.map(|init| self.lower_expr(init));
        self.nodes.alloc_object_binding_elem(elem.span, name, init)
    }

    fn lower_object_binding_name(
        &mut self,
        name: &'cx ast::ObjectBindingName<'cx>,
    ) -> ir::ObjectBindingName {
        match name {
            ast::ObjectBindingName::Shorthand(n) => {
                ir::ObjectBindingName::Shorthand(self.lower_ident(n))
            }
            ast::ObjectBindingName::Prop { prop_name, name } => {
                let prop_name = self.lower_prop_name(prop_name);
                let name = self.lower_binding(name);
                ir::ObjectBindingName::Prop { prop_name, name }
            }
        }
    }

    fn lower_prop_name(&mut self, prop_name: &'cx ast::PropName<'cx>) -> ir::PropName {
        match prop_name.kind {
            ast::PropNameKind::Ident(n) => ir::PropName::Ident(self.lower_ident(n)),
            ast::PropNameKind::StringLit { raw, .. } => {
                ir::PropName::StringLit(self.lower_string_lit(raw))
            }
            ast::PropNameKind::NumLit(n) => {
                ir::PropName::NumLit(self.nodes.alloc_num_lit(n.span, n.val))
            }
            ast::PropNameKind::Computed(n) => {
                let expr = self.lower_expr(n.expr);
                ir::PropName::Computed(self.nodes.alloc_computed_prop_name(n.span, expr))
            }
        }
    }

    fn lower_assign_expr(&mut self, expr: &'cx ast::AssignExpr<'cx>) -> ir::AssignExprID {
        let left = self.lower_expr(expr.left);
        let right = self.lower_expr(expr.right);
        self.nodes
            .alloc_assign_expr(expr.span, left, expr.op, right)
    }

    fn lower_ident(&mut self, ident: &'cx ast::Ident) -> ir::IdentID {
        let ty = self
            .checker
            .get_node_links(ident.id)
            .get_resolved_ty()
            .unwrap_or(self.checker.error_ty);
        self.nodes.alloc_ident(ty.id, ident.span, ident.name)
    }

    fn lower_bin_expr(&mut self, expr: &'cx ast::BinExpr<'cx>) -> ir::Expr {
        let left = self.lower_expr(expr.left);
        let right = self.lower_expr(expr.right);
        if let Some(lit) = shortcut_literal_binary_expression(self, left, right, expr.op) {
            ir::Expr::NumLit(lit)
        } else {
            ir::Expr::Bin(self.nodes.alloc_bin_expr(expr.span, left, expr.op, right))
        }
    }

    fn lower_expr(&mut self, expr: &'cx ast::Expr<'cx>) -> ir::Expr {
        use ast::ExprKind;
        match expr.kind {
            ExprKind::Ident(n) => ir::Expr::Ident(self.lower_ident(n)),
            ExprKind::Assign(n) => ir::Expr::Assign(self.lower_assign_expr(n)),
            ExprKind::Bin(n) => self.lower_bin_expr(n),
            ExprKind::This(n) => ir::Expr::This(self.nodes.alloc_this_expr(n.span)),
            ExprKind::BoolLit(n) => ir::Expr::BoolLit(self.nodes.alloc_bool_lit(n.span, n.val)),
            ExprKind::NumLit(n) => ir::Expr::NumLit(self.nodes.alloc_num_lit(n.span, n.val)),
            ExprKind::BigIntLit(n) => {
                ir::Expr::BigIntLit(self.nodes.alloc_bigint_lit(n.span, n.val.0, n.val.1))
            }
            ExprKind::StringLit(n) => ir::Expr::StringLit(self.lower_string_lit(n)),
            ExprKind::NoSubstitutionTemplateLit(n) => {
                ir::Expr::StringLit(self.nodes.alloc_string_lit(n.span, n.val, true))
            }
            ExprKind::NullLit(n) => ir::Expr::NullLit(self.nodes.alloc_null_lit(n.span)),
            ExprKind::RegExpLit(n) => {
                ir::Expr::RegExpLit(self.nodes.alloc_regexp_lit(n.span, n.val))
            }
            ExprKind::ArrayLit(n) => {
                let elems = n.elems.iter().map(|elem| self.lower_expr(elem)).collect();
                ir::Expr::ArrayLit(self.nodes.alloc_array_lit(n.span, elems))
            }
            ExprKind::Omit(n) => ir::Expr::Omit(self.lower_omit_expr(n)),
            ExprKind::Paren(n) => {
                let expr = self.lower_expr(n.expr);
                ir::Expr::Paren(self.nodes.alloc_paren_expr(n.span, expr))
            }
            ExprKind::Cond(n) => {
                let cond = self.lower_expr(n.cond);
                let when_true = self.lower_expr(n.when_true);
                let when_false = self.lower_expr(n.when_false);
                ir::Expr::Cond(
                    self.nodes
                        .alloc_cond_expr(n.span, cond, when_true, when_false),
                )
            }
            ExprKind::ObjectLit(n) => {
                let members = n
                    .members
                    .iter()
                    .filter_map(|m| match m.kind {
                        ast::ObjectMemberKind::Shorthand(n) => {
                            let name = self.lower_ident(n.name);
                            Some(ir::ObjectLitMember::Shorthand(
                                self.nodes.alloc_object_shorthand_member(n.span, name),
                            ))
                        }
                        ast::ObjectMemberKind::PropAssignment(n) => {
                            let key = self.lower_prop_name(n.name);
                            let init = self.lower_expr(n.init);
                            Some(ir::ObjectLitMember::Prop(
                                self.nodes.alloc_object_prop_member(n.span, key, init),
                            ))
                        }
                        ast::ObjectMemberKind::Method(n) => {
                            let name = self.lower_prop_name(n.name);
                            let params = self.lower_param_decls(n.params);
                            let body = self.lower_block_stmt(n.body);
                            Some(ir::ObjectLitMember::Method(
                                self.nodes
                                    .alloc_object_method_member(n.span, name, params, body),
                            ))
                        }
                        ast::ObjectMemberKind::SpreadAssignment(n) => {
                            let expr = self.lower_expr(n.expr);
                            Some(ir::ObjectLitMember::SpreadAssignment(
                                self.nodes.alloc_spread_assignment(n.span, expr),
                            ))
                        }
                        ast::ObjectMemberKind::Getter(n) => {
                            self.lower_getter_decl(n).map(ir::ObjectLitMember::Getter)
                        }
                        ast::ObjectMemberKind::Setter(n) => {
                            self.lower_setter_decl(n).map(ir::ObjectLitMember::Setter)
                        }
                    })
                    .collect();
                ir::Expr::ObjectLit(self.nodes.alloc_object_lit(n.span, members))
            }
            ExprKind::ExprWithTyArgs(n) => self.lower_expr(n.expr),
            ExprKind::Call(n) => {
                let expr = self.lower_expr(n.expr);
                let args = n.args.iter().map(|arg| self.lower_expr(arg)).collect();
                ir::Expr::Call(self.nodes.alloc_call_expr(n.span, expr, args))
            }
            ExprKind::Fn(n) => {
                let name = n.name.map(|name| self.lower_ident(name));
                let params = self.lower_param_decls(n.params);

                let saved = self.current;
                let graph = self.graph_arena.alloc_empty_graph();
                let bb = self.graph_arena.get_mut(graph).alloc_empty_basic_block();
                debug_assert!(bb == ir::BasicBlockID::ENTRY);
                self.current = (graph, bb);
                for stmt in self.lower_stmts(n.body.stmts) {
                    self.add_stmt_to_basic_block(stmt, (graph, bb));
                }
                self.current = saved;
                ir::Expr::Fn(self.nodes.alloc_fn_expr(n.span, name, params, graph))
            }
            ExprKind::Class(n) => {
                let name = n.name.map(|name| self.lower_ident(name));
                let extends = n
                    .extends
                    .map(|extends| self.lower_class_extends_clause(extends));
                let elems = self.lower_class_elems(n.elems);
                ir::Expr::Class(self.nodes.alloc_class_expr(n.span, name, extends, elems))
            }
            ExprKind::New(n) => {
                let expr = self.lower_expr(n.expr);
                let args = n
                    .args
                    .map(|args| {
                        args.iter()
                            .map(|arg| self.lower_expr(arg))
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();
                ir::Expr::New(self.nodes.alloc_new_expr(n.span, expr, args))
            }
            ExprKind::ArrowFn(n) => {
                let params = self.lower_param_decls(n.params);
                let saved = self.current;
                let graph = self.graph_arena.alloc_empty_graph();
                let bb = self.graph_arena.get_mut(graph).alloc_empty_basic_block();
                debug_assert!(bb == ir::BasicBlockID::ENTRY);
                self.current = (graph, bb);
                match n.body {
                    ast::ArrowFnExprBody::Block(n) => {
                        for stmt in self.lower_stmts(n.stmts) {
                            self.add_stmt_to_basic_block(stmt, (graph, bb));
                        }
                    }
                    ast::ArrowFnExprBody::Expr(n) => {
                        let expr = self.lower_expr(n);
                        let ret_expr = self.nodes.alloc_ret_stmt(n.span(), Some(expr));
                        self.add_stmt_to_basic_block(ir::Stmt::Ret(ret_expr), (graph, bb));
                    }
                };
                self.current = saved;
                let f = self.nodes.alloc_arrow_fn_expr(n.span, params, graph);
                ir::Expr::ArrowFn(f)
            }
            ExprKind::PrefixUnary(n) => self.lower_prefix_unary_expr(n),
            ExprKind::PostfixUnary(n) => {
                let expr = self.lower_expr(n.expr);
                ir::Expr::PostfixUnary(self.nodes.alloc_postfix_unary_expr(n.span, n.op, expr))
            }
            ExprKind::PropAccess(n) => ir::Expr::PropAccess(self.lower_prop_access_expr(n)),
            ExprKind::EleAccess(n) => {
                let expr = self.lower_expr(n.expr);
                let arg = self.lower_expr(n.arg);
                ir::Expr::EleAccess(self.nodes.alloc_ele_access_expr(n.span, expr, arg))
            }
            ExprKind::Super(n) => ir::Expr::Super(self.nodes.alloc_super_expr(n.span)),
            ExprKind::Typeof(n) => {
                let expr = self.lower_expr(n.expr);
                ir::Expr::Typeof(self.nodes.alloc_typeof_expr(n.span, expr))
            }
            ExprKind::Void(n) => {
                let expr = self.lower_expr(n.expr);
                ir::Expr::Void(self.nodes.alloc_void_expr(n.span, expr))
            }
            ExprKind::As(n) => self.lower_expr(n.expr),
            ExprKind::Satisfies(n) => self.lower_expr(n.expr),
            ExprKind::NonNull(n) => self.lower_expr(n.expr),
            ExprKind::Template(n) => ir::Expr::Template(self.lower_template_expr(n)),
            ExprKind::TaggedTemplate(n) => {
                let tag = self.lower_expr(n.tag);
                let tpl = self.lower_expr(n.tpl);
                ir::Expr::TaggedTemplate(self.nodes.alloc_tagged_template_expr(n.span, tag, tpl))
            }
            ExprKind::TyAssertion(n) => {
                let expr = self.lower_expr(n.expr);
                if let ast::ExprKind::ObjectLit(n) = n.expr.kind {
                    ir::Expr::Paren(self.nodes.alloc_paren_expr(n.span, expr))
                } else {
                    expr
                }
            }
            ExprKind::SpreadElement(n) => {
                let expr = self.lower_expr(n.expr);
                ir::Expr::SpreadElem(self.nodes.alloc_spread_element(n.span, expr))
            }
            ExprKind::JsxElem(n) => ir::Expr::JsxElem(self.lower_jsx_elem(n)),
            ExprKind::JsxSelfClosingElem(n) => {
                ir::Expr::JsxSelfClosingElem(self.lower_jsx_self_closing_elem(n))
            }
            ExprKind::JsxFrag(n) => ir::Expr::JsxFrag(self.lower_jsx_frag(n)),
            ExprKind::Delete(n) => {
                let expr = self.lower_expr(n.expr);
                ir::Expr::Delete(self.nodes.alloc_delete_expr(n.span, expr))
            }
            ExprKind::Await(n) => {
                let expr = self.lower_expr(n.expr);
                ir::Expr::Await(self.nodes.alloc_await_expr(n.span, expr))
            }
        }
    }

    fn lower_prefix_unary_expr(&mut self, n: &'cx ast::PrefixUnaryExpr<'cx>) -> ir::Expr {
        let expr = self.lower_expr(n.expr);

        if matches!(n.op, ast::PrefixUnaryOp::Excl)
            && let Some(boolean) = self.nodes.expr_as_literal_to_boolean(expr)
        {
            return ir::Expr::BoolLit(self.nodes.alloc_bool_lit(n.span, !boolean));
        } else if let ir::Expr::NumLit(lit) = expr {
            match n.op {
                ast::PrefixUnaryOp::Plus => return expr,
                ast::PrefixUnaryOp::Minus => {
                    let lit = self.nodes.get_num_lit(&lit);
                    let val = -lit.val();
                    return ir::Expr::NumLit(self.nodes.alloc_num_lit(n.span, val));
                }
                ast::PrefixUnaryOp::Tilde => {
                    let lit = self.nodes.get_num_lit(&lit);
                    let int = js_double_to_int32(lit.val());
                    let val = !int as f64;
                    return ir::Expr::NumLit(self.nodes.alloc_num_lit(n.span, val));
                }
                _ => {}
            }
        }

        ir::Expr::PrefixUnary(self.nodes.alloc_prefix_unary_expr(n.span, n.op, expr))
    }

    fn lower_jsx_frag(&mut self, n: &'cx ast::JsxFrag<'cx>) -> ir::JsxFragID {
        let opening = self.nodes.alloc_jsx_opening_frag(n.opening_frag.span);
        let children = self.lower_jsx_children(n.children);
        let closing = self.nodes.alloc_jsx_closing_frag(n.closing_frag.span);
        self.nodes
            .alloc_jsx_frag(n.span, opening, children, closing)
    }

    fn lower_jsx_self_closing_elem(
        &mut self,
        n: &'cx ast::JsxSelfClosingElem<'cx>,
    ) -> ir::JsxSelfClosingElemID {
        let tag_name = self.lower_jsx_tag_name(&n.tag_name);
        let attrs = self.lower_jsx_attrs(&n.attrs);
        self.nodes
            .alloc_jsx_self_closing_elem(n.span, tag_name, attrs)
    }

    fn lower_jsx_elem(&mut self, n: &'cx ast::JsxElem<'cx>) -> ir::JsxElemID {
        let opening = {
            let tag_name = self.lower_jsx_tag_name(&n.opening_elem.tag_name);
            let attrs = self.lower_jsx_attrs(&n.opening_elem.attrs);
            self.nodes
                .alloc_jsx_opening_elem(n.opening_elem.span, tag_name, attrs)
        };
        let children = self.lower_jsx_children(n.children);
        let closing = {
            let tag_name = self.lower_jsx_tag_name(&n.closing_elem.tag_name);
            self.nodes
                .alloc_jsx_closing_elem(n.closing_elem.span, tag_name)
        };
        self.nodes
            .alloc_jsx_elem(n.span, opening, children, closing)
    }

    fn lower_jsx_children(&mut self, n: &'cx [ast::JsxChild<'cx>]) -> Vec<ir::JsxChild> {
        n.iter().map(|child| self.lower_jsx_child(child)).collect()
    }

    fn lower_jsx_child(&mut self, n: &'cx ast::JsxChild<'cx>) -> ir::JsxChild {
        match n {
            ast::JsxChild::Text(n) => ir::JsxChild::Text(self.lower_jsx_text(n)),
            ast::JsxChild::Expr(n) => ir::JsxChild::Expr(self.lower_jsx_expr(n)),
            ast::JsxChild::Elem(n) => ir::JsxChild::Elem(self.lower_jsx_elem(n)),
            ast::JsxChild::SelfClosingEle(n) => {
                ir::JsxChild::SelfClosingEle(self.lower_jsx_self_closing_elem(n))
            }
            ast::JsxChild::Frag(n) => ir::JsxChild::Frag(self.lower_jsx_frag(n)),
        }
    }

    fn lower_jsx_expr(&mut self, n: &'cx ast::JsxExpr<'cx>) -> ir::JsxExprID {
        let expr = n.expr.map(|e| self.lower_expr(e));
        self.nodes.alloc_jsx_expr(n.span, n.dotdotdot_token, expr)
    }

    fn lower_jsx_text(&mut self, n: &'cx ast::JsxText) -> ir::JsxTextID {
        self.nodes
            .alloc_jsx_text(n.span, n.text, n.contains_only_trivia_whitespace)
    }

    fn lower_jsx_attr(&mut self, n: &'cx ast::JsxAttr<'cx>) -> ir::JsxAttr {
        match n {
            ast::JsxAttr::Spread(n) => {
                let expr = self.lower_expr(n.expr);
                ir::JsxAttr::Spread(self.nodes.alloc_jsx_spread_attr(n.span, expr))
            }
            ast::JsxAttr::Named(n) => {
                let name = match n.name {
                    ast::JsxAttrName::Ident(n) => ir::JsxAttrName::Ident(self.lower_ident(n)),
                    ast::JsxAttrName::Ns(n) => ir::JsxAttrName::Ns(self.lower_jsx_ns_name(n)),
                };
                let init = n.init.map(|init| match init {
                    ast::JsxAttrValue::StringLit(n) => {
                        ir::JsxAttrValue::StringLit(self.lower_string_lit(n))
                    }
                    ast::JsxAttrValue::Expr(n) => ir::JsxAttrValue::Expr(self.lower_jsx_expr(n)),
                    ast::JsxAttrValue::Ele(n) => ir::JsxAttrValue::Ele(self.lower_jsx_elem(n)),
                    ast::JsxAttrValue::SelfClosingEle(n) => {
                        ir::JsxAttrValue::SelfClosingEle(self.lower_jsx_self_closing_elem(n))
                    }
                    ast::JsxAttrValue::Frag(n) => ir::JsxAttrValue::Frag(self.lower_jsx_frag(n)),
                });
                ir::JsxAttr::Named(self.nodes.alloc_jsx_named_attr(n.span, name, init))
            }
        }
    }

    fn lower_jsx_attrs(&mut self, n: &'cx ast::JsxAttrs<'cx>) -> Vec<ir::JsxAttr> {
        n.iter().map(|attr| self.lower_jsx_attr(attr)).collect()
    }

    fn lower_prop_access_expr(&mut self, n: &'cx ast::PropAccessExpr<'cx>) -> ir::PropAccessExprID {
        let expr = self.lower_expr(n.expr);
        let name = self.lower_ident(n.name);
        self.nodes
            .alloc_prop_access_expr(n.span, expr, n.question_dot, name)
    }

    fn lower_jsx_ns_name(&mut self, n: &'cx ast::JsxNsName<'cx>) -> ir::JsxNsNameID {
        let ns = self.lower_ident(n.ns);
        let name = self.lower_ident(n.name);
        self.nodes.alloc_jsx_ns_name(n.span, ns, name)
    }

    fn lower_jsx_tag_name(&mut self, tag_name: &ast::JsxTagName<'cx>) -> ir::JsxTagName {
        match tag_name {
            ast::JsxTagName::Ident(n) => ir::JsxTagName::Ident(self.lower_ident(n)),
            ast::JsxTagName::This(n) => ir::JsxTagName::This(self.nodes.alloc_this_expr(n.span)),
            ast::JsxTagName::Ns(n) => ir::JsxTagName::Ns(self.lower_jsx_ns_name(n)),
            ast::JsxTagName::PropAccess(n) => {
                ir::JsxTagName::PropAccess(self.lower_prop_access_expr(n))
            }
        }
    }

    fn lower_template_expr(&mut self, n: &'cx ast::TemplateExpr) -> ir::TemplateExprID {
        let head = self.nodes.alloc_template_head(n.head.span, n.head.text);
        let spans = n
            .spans
            .iter()
            .map(|span| {
                let expr = self.lower_expr(span.expr);
                self.nodes
                    .alloc_template_span(span.span, expr, span.text, span.is_tail)
            })
            .collect();
        self.nodes.alloc_template_expr(n.span, head, spans)
    }
}

fn shortcut_literal_binary_expression(
    ctx: &mut LoweringCtx,
    x: ir::Expr,
    y: ir::Expr,
    op: ast::BinOp,
) -> Option<ir::NumLitID> {
    if let ir::Expr::NumLit(x) = x
        && let ir::Expr::NumLit(y) = y
    {
        let x = ctx.nodes.get_num_lit(&x);
        let y = ctx.nodes.get_num_lit(&y);
        let span = || Span::new(x.span().lo(), y.span().hi(), ModuleID::TRANSIENT);
        match op.kind {
            ast::BinOpKind::Add => {
                let val = x.val() + y.val();
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, val));
            }
            ast::BinOpKind::Sub => {
                let val = x.val() - y.val();
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, val));
            }
            ast::BinOpKind::Mul => {
                let val = x.val() * y.val();
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, val));
            }
            ast::BinOpKind::Div => {
                let val = x.val() / y.val();
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, val));
            }
            ast::BinOpKind::Mod => {
                let val = x.val() % y.val();
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, val));
            }
            ast::BinOpKind::BitOr => {
                let l = js_double_to_int32(x.val());
                let r = js_double_to_int32(y.val());
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, l.bitor(r) as f64));
            }
            ast::BinOpKind::BitAnd => {
                let l = js_double_to_int32(x.val());
                let r = js_double_to_int32(y.val());
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, l.bitand(r) as f64));
            }
            ast::BinOpKind::BitXor => {
                let l = js_double_to_int32(x.val());
                let r = js_double_to_int32(y.val());
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, l.bitxor(r) as f64));
            }
            ast::BinOpKind::Shl => {
                let l = js_double_to_int32(x.val());
                let r = js_double_to_int32(y.val());
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, l.shl(r) as f64));
            }
            ast::BinOpKind::Shr => {
                let l = js_double_to_int32(x.val());
                let r = js_double_to_int32(y.val());
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, l.shr(r) as f64));
            }
            ast::BinOpKind::UShr => {
                let l = js_double_to_int32(x.val());
                let r = js_double_to_int32(y.val());
                let span = span();
                return Some(
                    ctx.nodes
                        .alloc_num_lit(span, l.wrapping_shr(r as u32) as f64),
                );
            }
            ast::BinOpKind::Exp => {
                let val = x.val().powf(y.val());
                let span = span();
                return Some(ctx.nodes.alloc_num_lit(span, val));
            }
            _ => (),
        }
    }

    None
}

// fn get_representation_for_binary_expression(
//     ctx: &mut LoweringCtx,
//     x: ir::Expr,
//     y: ir::Expr,
//     op: ast::BinOp,
// ) -> Option<ir::Expr> {
//     if !ctx.scope_flags.contains(ScopeFlags::FUNCTION) {
//         return None;
//     }
//     if let ir::Expr::Ident(x) = x
//         && let x = ctx.nodes.get_ident(&x)
//         && let Some(ty::NumberLitTy { val, .. }) = ctx.checker.ty(i.ty()).kind.as_number_lit()
//         && let ir::Expr::NumLit(y) = y
//     {
//         let y = ctx.nodes.get_num_lit(&y);
//     }
//     None
// }
