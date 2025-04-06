mod errors;

use bolt_ts_ast::{self as ast};
use bolt_ts_path::NormalizePath;

use crate::parser::FileReference;

use super::parser;
use super::{ModuleArena, ModuleID};

use std::sync::{Arc, Mutex};

use bolt_ts_atom::{AtomId, AtomMap};
use bolt_ts_fs::PathId;
use bolt_ts_resolve::RResult;
use bolt_ts_utils::fx_hashmap_with_capacity;
use rayon::prelude::*;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ModuleRes {
    Err,
    Res(ModuleID),
}

pub struct ModuleGraph {
    deps: FxHashMap<ModuleID, FxHashMap<AtomId, ModuleRes>>,
    diags: Vec<bolt_ts_errors::Diag>,
}

impl ModuleGraph {
    fn add_dep(&mut self, from: bolt_ts_span::ModuleID, by: AtomId, to: ModuleRes) {
        if let Some(from) = self.deps.get_mut(&from) {
            if let Some(prev) = from.insert(by, to) {
                assert_eq!(prev, to);
            }
        } else {
            let mut map = fx_hashmap_with_capacity(32);
            map.insert(by, to);
            self.deps.insert(from, map);
        }
    }

    pub fn get_dep(&self, from: bolt_ts_span::ModuleID, by: AtomId) -> Option<ModuleRes> {
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
    default_lib_dir: &std::path::Path,
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
        let modules = parser::parse_parallel(
            atoms.clone(),
            herd,
            resolving.as_slice(),
            module_arena,
            &default_lib_dir,
        )
        .map(|(module_id, mut parse_result)| {
            let file_path = module_arena.get_path(module_id);
            debug_assert!(file_path.is_normalized());
            let base_dir = file_path.parent().unwrap();
            debug_assert!(base_dir.is_normalized());
            let base_dir = PathId::get(base_dir);
            let imports = std::mem::take(&mut parse_result.imports);
            // TODO: filter imports
            let deps = imports
                .into_par_iter()
                .map(|s| (s.id, resolver.resolve(base_dir, s.val)))
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
            for lib_reference in &parse_result.lib_references {
                match resolved.get(lib_reference) {
                    Some(_) => {}
                    None => {
                        if next.contains_key(lib_reference) {
                            continue;
                        }
                        let p = std::path::PathBuf::from(atoms.get((*lib_reference).into()));
                        let content = fs.read_file(p.as_path(), atoms).unwrap();
                        let to = module_arena.new_module_with_content(p, true, content, atoms);
                        next.insert(*lib_reference, to);
                    }
                }
            }

            parser.insert(id, parse_result);

            for (ast_id, dep) in deps {
                let module_name_node = parser.node(ast_id);
                let module_name = match module_name_node {
                    ast::Node::StringLit(lit) => lit.val,
                    // TODO: NoSubstitutionTemplateLiteral
                    _ => unreachable!(),
                };
                let m = ast_id.module();
                let Ok(dep) = dep else {
                    mg.add_dep(m, module_name, ModuleRes::Err);
                    mg.push_error(Box::new(
                        errors::CannotFindModuleOrItsCorrespondingTypeDeclarations {
                            span: module_name_node.span(),
                            module_name: atoms.get(module_name).to_string(),
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
                mg.add_dep(m, module_name, ModuleRes::Res(to));
            }
        }

        resolving = next.into_values().collect();
    }
    mg
}

pub fn resolve_external_module_name(
    mg: &ModuleGraph,
    module_spec: ast::NodeID,
    p: &parser::Parser<'_>,
) -> Option<super::bind::SymbolID> {
    let from = module_spec.module();
    let name = match p.node(module_spec) {
        ast::Node::StringLit(lit) => lit.val,
        // TODO: NoSubstitutionTemplateLiteral
        _ => unreachable!(),
    };
    let Some(dep) = mg.get_dep(from, name) else {
        unreachable!()
    };

    match dep {
        ModuleRes::Err => None,
        ModuleRes::Res(module_id) => Some(super::bind::SymbolID::container(module_id)),
    }
}
