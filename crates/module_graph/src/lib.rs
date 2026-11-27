mod errors;

use bolt_ts_ast::{self as ast};
use bolt_ts_atom::{Atom, AtomIntern};
use bolt_ts_fs::PathId;
use bolt_ts_middle::Extension;
use bolt_ts_module_resolve::get_resolution_mode_for_usage_location;
use bolt_ts_module_resolve::{ResolutionMode, ResolveFlags};
use bolt_ts_parser::ParsedMapState;
use bolt_ts_span::{ModuleArena, ModuleID};
use bolt_ts_utils::fx_hashmap_with_capacity;
use bolt_ts_utils::path::NormalizePath;

use rayon::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};

use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ModuleRes {
    Err,
    Res(ModuleID),
}

pub struct ModuleGraph {
    deps: FxHashMap<ModuleID, FxHashMap<Atom, ModuleRes>>,
    diags: Vec<bolt_ts_errors::Diag>,
}

impl ModuleGraph {
    fn add_dep(&mut self, from: bolt_ts_span::ModuleID, by: Atom, to: ModuleRes) {
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

    pub fn get_dep(&self, from: bolt_ts_span::ModuleID, by: Atom) -> Option<ModuleRes> {
        self.deps.get(&from).and_then(|map| map.get(&by).copied())
    }

    pub fn push_error(&mut self, diag: bolt_ts_errors::BoxedDiag) {
        self.diags.push(bolt_ts_errors::Diag { inner: diag });
    }

    pub fn steal_errors(&mut self) -> Vec<bolt_ts_errors::Diag> {
        std::mem::take(&mut self.diags)
    }
}

#[derive(Debug, Clone, Copy)]
struct ResolveKey<'cx> {
    module_name_node_id: ast::NodeID,
    module_name: Atom,
    containing_file: bolt_ts_module_resolve::ContainingFile,
    mode: Option<ResolutionMode>,
    options: bolt_ts_module_resolve::ResolverOptions<'cx>,
}

impl std::hash::Hash for ResolveKey<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.module_name.hash(state);
        self.containing_file.hash(state);
        self.mode.hash(state);
        self.options.hash(state);
    }
}

impl PartialEq for ResolveKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.module_name == other.module_name
            && self.containing_file == other.containing_file
            && self.mode == other.mode
            && self.options == other.options
    }
}

impl Eq for ResolveKey<'_> {}

#[allow(clippy::too_many_arguments)]
pub fn build_graph<'cx>(
    module_arena: &mut ModuleArena,
    modules: &[ModuleID],
    atoms: Arc<Mutex<AtomIntern>>,
    default_lib_dir: &std::path::Path,
    herd: &'cx bolt_ts_arena::bumpalo_herd::Herd,
    parsed: &mut ParsedMapState<'cx>,
    fs: Arc<Mutex<impl bolt_ts_fs::CachedFileSystem>>,
    options: &bolt_ts_config::NormalizedTsConfig,
) -> ModuleGraph {
    let compiler_option_flags = *options.compiler_options().flags();
    let target = *options.compiler_options().target();
    // resolve
    let mut flags = ResolveFlags::empty();
    if options.compiler_options().preserve_symlinks() {
        flags.insert(ResolveFlags::PRESERVE_SYMLINKS);
    };
    if options.compiler_options().no_dts_resolution() {
        flags.insert(ResolveFlags::NO_DTS_RESOLUTION);
    }
    if options.compiler_options().resolve_json_module() {
        flags.insert(ResolveFlags::RESOLVE_JSON_MODULE);
    }
    if options.compiler_options().resolve_package_json_exports() {
        flags.insert(ResolveFlags::RESOLVE_PACKAGE_JSON_EXPORTS);
    }
    if options.compiler_options().resolve_package_json_imports() {
        flags.insert(ResolveFlags::RESOLVE_PACKAGE_JSON_IMPORTS);
    }

    let cache = bolt_ts_module_resolve::ModuleResolutionCache::new();

    let mut resolved = fx_hashmap_with_capacity(2048);
    let mut resolving = modules.to_vec();
    let mut mg = ModuleGraph {
        deps: fx_hashmap_with_capacity(modules.len() * 32),
        diags: vec![],
    };
    let module_resolution = *options.compiler_options().module_resolution();
    while !resolving.is_empty() {
        debug_assert!(resolving.is_sorted_by_key(|m| m.as_u32()));

        let list = bolt_ts_parser::parse_parallel(
            atoms.clone(),
            herd,
            resolving.as_slice(),
            module_arena,
            default_lib_dir,
            compiler_option_flags,
            target,
        )
        .map(|(module_id, mut parse_result)| {
            let file_path = module_arena.get_path(module_id);
            debug_assert!(file_path.is_normalized());
            let file_ext =
                Extension::extension_of_file_name(file_path.as_os_str().as_encoded_bytes());
            let base_dir = file_path.parent().unwrap();
            debug_assert!(base_dir.is_normalized());
            let base_dir = PathId::get(base_dir, atoms.lock().as_mut().unwrap());
            let imports = std::mem::take(&mut parse_result.imports);
            let resolution_mode =
                get_resolution_mode_for_usage_location(file_ext, Some(options.compiler_options()));
            let group = imports
                .iter()
                .map(|import| ResolveKey {
                    module_name: import.module_name.val,
                    module_name_node_id: import.module_name.id,
                    containing_file: bolt_ts_module_resolve::ContainingFile::new(base_dir),
                    mode: resolution_mode,
                    options: bolt_ts_module_resolve::ResolverOptions {
                        module_resolution,
                        custom_conditions: options.compiler_options().custom_conditions(),
                        flags,
                    },
                })
                .collect::<Vec<_>>();
            (module_id, parse_result, group)
        })
        .collect::<Vec<_>>();
        let resolving_dependencies = list
            .iter()
            .flat_map(|(_, _, group)| group.clone())
            .collect::<FxHashSet<_>>();
        let resolved_dependencies = resolving_dependencies
            .into_par_iter()
            .map(|key| {
                let res = bolt_ts_module_resolve::Resolver::resolve_module_name(
                    key.module_name,
                    key.containing_file,
                    key.options,
                    &cache,
                    &atoms,
                    &fs,
                    key.mode,
                );
                (key, res)
            })
            .collect::<FxHashMap<_, _>>();

        for item in resolving {
            let p = module_arena.get_path(item);
            debug_assert!(p.is_normalized());
            let path_id = PathId::get(p, atoms.lock().as_mut().unwrap());
            resolved.insert(path_id, item);
        }

        let atoms = &mut atoms.lock().unwrap();
        let fs = &mut fs.lock().unwrap();

        let mut next = indexmap::IndexMap::with_capacity(list.len() * 4);
        for (id, parse_result, group) in list {
            for lib_reference in &parse_result.lib_references {
                debug_assert!(lib_reference.is_normalized());
                let id = PathId::new(lib_reference, atoms);
                match resolved.get(&id) {
                    Some(_) => {}
                    None => {
                        if next.contains_key(&id) {
                            continue;
                        }
                        let content = fs.read_file(lib_reference, atoms).unwrap();
                        let to = module_arena.new_module_with_content_within_preserve(
                            lib_reference.to_path_buf(),
                            true,
                            content,
                            atoms,
                        );
                        next.insert(id, to);
                    }
                }
            }

            if module_arena.is_preserve(id) {
                parsed.insert_within_preserve(id, parse_result);
            } else {
                parsed.insert(id, parse_result);
            }

            for item in group {
                let ast_id = item.module_name_node_id;
                let dep = resolved_dependencies[&item];
                let module_name_node = parsed.node(ast_id);
                let module_name = match module_name_node {
                    ast::Node::StringLit(lit) => lit.val,
                    ast::Node::NoSubstitutionTemplateLit(lit) => lit.val,
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
