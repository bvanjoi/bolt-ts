use bolt_ts_ast::{self as ast, keyword};
use bolt_ts_ast_visitor::{ControlFlow, Visitor};
use bolt_ts_atom::{Atom, AtomIntern};

use std::sync::{Arc, Mutex};

use super::ImportInfo;

pub(super) fn collect_deps<'cx>(
    is_declaration: bool,
    is_external_module_file: bool,
    root: &'cx ast::Program<'cx>,
    atoms: Arc<Mutex<AtomIntern>>,
) -> CollectDepsResult<'cx> {
    let mut visitor = CollectDepsVisitor {
        in_ambient_module: false,
        is_declaration,
        is_external_module_file,
        atoms,
        imports: Vec::with_capacity(32),
        ambient_modules: Vec::with_capacity(8),
        module_augmentations: Vec::with_capacity(8),
    };
    visitor.visit_program(root);
    CollectDepsResult {
        imports: visitor.imports,
        module_augmentations: visitor.module_augmentations,
        ambient_modules: visitor.ambient_modules,
    }
}

struct CollectDepsVisitor<'cx> {
    is_declaration: bool,
    in_ambient_module: bool,
    is_external_module_file: bool,
    atoms: Arc<Mutex<AtomIntern>>,

    imports: Vec<ImportInfo<'cx>>,
    module_augmentations: Vec<ast::NodeID>,
    ambient_modules: Vec<Atom>,
}

pub(super) struct CollectDepsResult<'cx> {
    pub(super) imports: Vec<ImportInfo<'cx>>,
    pub(super) module_augmentations: Vec<ast::NodeID>,
    pub(super) ambient_modules: Vec<Atom>,
}

impl<'cx> Visitor<'cx> for CollectDepsVisitor<'cx> {
    type Result = bolt_ts_ast_visitor::ControlFlow;
    fn visit_stmt(&mut self, node: &'cx ast::Stmt<'cx>) -> ControlFlow {
        let module_name = match node.kind {
            ast::StmtKind::Import(n) => Some(n.module),
            ast::StmtKind::Export(n) => n.module_spec(),
            // TODO: import equal
            ast::StmtKind::BlockModule(n) => {
                if n.is_ambient()
                    && (self.in_ambient_module
                        || n.modifiers
                            .is_some_and(|ms| ms.flags.contains(ast::ModifierFlags::AMBIENT))
                        || self.is_declaration)
                {
                    let name = match n.name {
                        bolt_ts_ast::ModuleName::Ident(_) => {
                            assert!(n.is_global_argument);
                            keyword::IDENT_GLOBAL
                        }
                        bolt_ts_ast::ModuleName::StringLit(lit) => lit.val,
                    };
                    if self.is_external_module_file
                        || (self.in_ambient_module
                            && name != keyword::IDENT_GLOBAL
                            && !bolt_ts_path::is_external_module_relative(
                                self.atoms.lock().unwrap().get(name),
                            ))
                    {
                        self.module_augmentations.push(n.name.id());
                    } else if !self.in_ambient_module {
                        if self.is_declaration {
                            self.ambient_modules.push(name);
                        }

                        if let Some(block) = n.block {
                            self.in_ambient_module = true;
                            for stmt in block.stmts {
                                if self.visit_stmt(stmt).is_break() {
                                    self.in_ambient_module = false;
                                    return ControlFlow::Break;
                                }
                            }
                            self.in_ambient_module = false;
                        }
                    }
                }
                return ControlFlow::Continue;
            }
            _ => return ControlFlow::Continue,
        };
        if let Some(module_name) = module_name
            && !(self.in_ambient_module
                && bolt_ts_path::is_external_module_relative(
                    self.atoms.lock().unwrap().get(module_name.val),
                ))
        {
            self.imports.push(ImportInfo {
                module_name,
                // kind: todo!(),
            });
        }
        // TODO: use_uri_style_node_core_modules
        ControlFlow::Continue
    }
}
