mod errors;

use bolt_ts_ast as ast;
use bolt_ts_ast::Visitor;
use normalize_path::NormalizePath;

use super::parser;
use super::parser::parse_parallel;
use super::{ModuleArena, ModuleID};

use std::sync::{Arc, Mutex};

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_fs::PathId;
use bolt_ts_resolve::RResult;
use bolt_ts_utils::{fx_hashmap_with_capacity, fx_hashset_with_capacity};
use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Copy)]
pub enum ModuleRes {
    Err,
    Res(ModuleID),
}

pub struct ModuleGraph {
    deps: FxHashMap<ModuleID, FxHashMap<ast::NodeID, ModuleRes>>,
    diags: Vec<bolt_ts_errors::Diag>,
}

impl ModuleGraph {
    fn add_dep(&mut self, from: ast::NodeID, to: ModuleRes) {
        let by = from;
        let from = by.module();
        assert_eq!(from, by.module());
        if let Some(from) = self.deps.get_mut(&from) {
            let prev = from.insert(by, to);
            assert!(prev.is_none());
        } else {
            let mut map = fx_hashmap_with_capacity(32);
            map.insert(by, to);
            self.deps.insert(from, map);
        }
    }

    pub fn get_dep(&self, from: ast::NodeID) -> Option<ModuleRes> {
        let by = from;
        let from = by.module();
        self.deps.get(&from).and_then(|map| map.get(&by).copied())
    }

    pub fn push_error(&mut self, diag: crate::Diag) {
        self.diags.push(bolt_ts_errors::Diag { inner: diag });
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        std::mem::take(&mut self.diags)
    }
}

pub(super) fn build_graph<'cx>(
    module_arena: &mut ModuleArena,
    list: &[ModuleID],
    atoms: Arc<Mutex<AtomMap<'cx>>>,
    herd: &'cx bumpalo_herd::Herd,
    parser: &mut parser::Parser<'cx>,
    fs: impl bolt_ts_fs::CachedFileSystem,
) -> ModuleGraph {
    let fs = Arc::new(Mutex::new(fs));
    let resolver = bolt_ts_resolve::Resolver::new(fs.clone(), atoms.clone());
    let mut resolved = fx_hashmap_with_capacity(2048);
    let mut resolving = list.to_vec();
    let mut mg = ModuleGraph {
        deps: fx_hashmap_with_capacity(list.len() * 32),
        diags: vec![],
    };
    while !resolving.is_empty() {
        debug_assert!(resolving.is_sorted_by_key(|m| m.as_u32()));
        struct ResolvedModule<'cx> {
            id: ModuleID,
            parse_result: parser::ParseResult<'cx>,
            deps: Vec<(ast::NodeID, RResult<PathId>)>,
        }
        let modules = parse_parallel(atoms.clone(), herd, resolving.as_slice(), module_arena)
            .map(|(module_id, parse_result)| {
                let deps = collect_deps(parse_result.root());
                let file_path = module_arena.get_path(module_id);
                let base_dir = file_path.parent().unwrap();
                let base_dir = PathId::get(base_dir);
                let deps = deps
                    .into_par_iter()
                    .map(|(node_id, dep)| (node_id, resolver.resolve(base_dir, dep)))
                    .collect::<Vec<_>>();
                ResolvedModule {
                    id: module_id,
                    parse_result,
                    deps,
                }
            })
            .collect::<Vec<_>>();

        for item in resolving {
            let p = module_arena.get_path(item);
            debug_assert!(p.is_normalized());
            let path_id = PathId::get(p);
            resolved.insert(path_id, item);
        }

        let atoms = &mut atoms.lock().unwrap();
        let fs = &mut fs.lock().unwrap();

        let mut next = indexmap::IndexMap::with_capacity(modules.len() * 32);
        for ResolvedModule {
            id,
            parse_result,
            deps,
        } in modules
        {
            parser.insert(id, parse_result);

            for (ast_id, dep) in deps {
                let Ok(dep) = dep else {
                    mg.add_dep(ast_id, ModuleRes::Err);
                    let module_name = parser.node(ast_id).module_name().unwrap();
                    mg.push_error(Box::new(
                        errors::CannotFindModuleOrItsCorrespondingTypeDeclarations {
                            span: module_name.span,
                            module_name: atoms.get(module_name.val).to_string(),
                        },
                    ));
                    continue;
                };
                let to = match resolved.get(&dep) {
                    Some(to) => *to,
                    None => {
                        if let Some(to) = next.get(&dep).copied() {
                            to
                        } else {
                            let p = std::path::PathBuf::from(atoms.get(dep.into()));
                            let content = fs.read_file(p.as_path(), atoms).unwrap();
                            let to = module_arena.new_module_with_content(p, false, content, atoms);
                            next.insert(dep, to);
                            to
                        }
                    }
                };
                mg.add_dep(ast_id, ModuleRes::Res(to));
            }
        }

        resolving = next.into_values().collect();
    }

    mg
}

fn collect_deps(root: &ast::Program) -> FxHashSet<(ast::NodeID, AtomId)> {
    let mut visitor = CollectDepsVisitor {
        deps: fx_hashset_with_capacity(32),
    };
    visitor.visit_program(root);
    visitor.deps
}

struct CollectDepsVisitor {
    deps: FxHashSet<(ast::NodeID, AtomId)>,
}

impl<'cx> ast::Visitor<'cx> for CollectDepsVisitor {
    fn visit_stmt(&mut self, node: &'cx ast::Stmt<'cx>) {
        match node.kind {
            ast::StmtKind::Import(n) => {
                let prev = self.deps.insert((n.id, n.module.val));
                assert!(prev);
            }
            ast::StmtKind::Export(n) => {
                let m = match n.clause.kind {
                    bolt_ts_ast::ExportClauseKind::Glob(n) => Some(n.module.val),
                    bolt_ts_ast::ExportClauseKind::Ns(n) => Some(n.module.val),
                    bolt_ts_ast::ExportClauseKind::Specs(n) => n.module.map(|n| n.val),
                };
                if let Some(m) = m {
                    let prev = self.deps.insert((n.id, m));
                    assert!(prev);
                }
            }
            _ => {}
        }
    }
}
