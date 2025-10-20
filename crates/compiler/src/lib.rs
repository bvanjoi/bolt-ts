mod cli;
mod diag;
mod emit_declaration;
mod r#trait;
mod wf;

use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use crate::emit_declaration::emit_declaration_parallel;

use self::cli::get_filenames;
use self::wf::well_formed_check_parallel;

use bolt_ts_ast::TokenKind;
use bolt_ts_atom::AtomIntern;
use bolt_ts_binder::bind_parallel;
use bolt_ts_binder::{Binder, ResolveResult};
use bolt_ts_binder::{BinderResult, MergeGlobalSymbolResult};
use bolt_ts_config::{CompilerOptionFlags, NormalizedTsConfig};
use bolt_ts_early_resolve::early_resolve_parallel;
use bolt_ts_fs::{CachedFileSystem, read_file_with_encoding};
use bolt_ts_middle::Diag;
use bolt_ts_optimize::{IrOutput, optimize_and_js_emit};
use bolt_ts_parser::{ParseResultForGraph, ParsedMap};
use bolt_ts_span::{ModuleArena, ModuleID};
use bolt_ts_utils::path::NormalizePath;

use rayon::prelude::*;
use rustc_hash::FxHashMap;

pub const DEFAULT_TSCONFIG: &str = "tsconfig.json";

#[derive(Debug, Clone)]
pub struct OutputFile {
    js_code: String,
    dts_code: String,
}

pub struct Output {
    pub root: PathBuf,
    pub module_arena: ModuleArena,
    pub diags: Vec<bolt_ts_errors::Diag>,
    pub types_len: usize,

    pub ir: Vec<(ModuleID, IrOutput)>,
    pub files: Vec<(ModuleID, OutputFile)>,
}

pub fn current_exe_dir() -> std::path::PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

pub fn output_files(
    root: &std::path::Path,
    tsconfig: &NormalizedTsConfig,
    module_arena: &ModuleArena,
    files: Vec<(ModuleID, OutputFile)>,
) -> FxHashMap<PathBuf, String> {
    let p = |m: ModuleID| module_arena.get_path(m);
    match tsconfig.compiler_options().out_dir() {
        bolt_ts_config::OutDir::OwnRoot => files
            .into_iter()
            .flat_map(|(m, file)| {
                let p = p(m);
                let js_output_path = p.with_extension("js");
                let dts_output_path = p.with_extension("d.ts");
                vec![
                    (js_output_path, file.js_code),
                    (dts_output_path, file.dts_code),
                ]
            })
            .collect(),
        bolt_ts_config::OutDir::Custom(dir) => {
            let dir = root.join(dir);
            files
                .into_iter()
                .flat_map(|(m, file)| {
                    let file_path = p(m);
                    let file_name = file_path.file_name().unwrap();
                    let p = dir.join(file_name);
                    let js_output_path = p.with_extension("js");
                    let dts_output_path = p.with_extension("d.ts");
                    vec![
                        (js_output_path, file.js_code),
                        (dts_output_path, file.dts_code),
                    ]
                })
                .collect()
        }
    }
}

pub fn init_atom() -> AtomIntern {
    #[cfg(debug_assertions)]
    for idx in 0..bolt_ts_ast::keyword::KEYWORDS.len() {
        let t = bolt_ts_ast::keyword_idx_to_token(idx);
        if t == TokenKind::Var {
            assert_eq!(t as u8 + 1, TokenKind::Let as u8);
            assert_eq!(t as u8 + 2, TokenKind::Const as u8);
        }
    }
    bolt_ts_ast::keyword::init_atom_map()
}

pub fn eval_from_memory_path(
    cwd: String,
    default_lib_dir: String,
    mut files: indexmap::IndexMap<String, String>,
) -> Output {
    let mut atoms = self::init_atom();
    let root = std::path::PathBuf::from(cwd);
    add_default_libs(&mut files, &default_lib_dir);
    let default_lib_dir = std::path::PathBuf::from(default_lib_dir);
    let mut fs = bolt_ts_fs::MemoryFS::new(files.into_iter(), &mut atoms).unwrap();
    let tsconfig: bolt_ts_config::RawTsConfig = if let Ok(raw_tsconfig) =
        fs.read_file(std::path::Path::new("/tsconfig.json"), &mut atoms)
    {
        serde_json::from_str(atoms.get(raw_tsconfig)).unwrap()
    } else {
        Default::default()
    };
    let default_libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| default_lib_dir.join(filename))
        .collect::<Vec<_>>();
    let tsconfig = tsconfig.normalize();
    return eval_with_fs(root, &tsconfig, default_lib_dir, default_libs, fs, atoms);

    fn add_default_libs(files: &mut indexmap::IndexMap<String, String>, default_lib_dir: &str) {
        macro_rules! add_default_lib_file {
        ($($name: literal),* $( , )?) => {
            $(
                let code = include_str!(concat!("../../libs/src/declared_file/", $name));
                let prev = files.insert(format!("{default_lib_dir}/{}", $name), code.to_string());
                assert!(prev.is_none());
            )*
        };
    }
        add_default_lib_file!(
            "lib.d.ts",
            "lib.decorators.d.ts",
            "lib.decorators.legacy.d.ts",
            "lib.dom.asynciterable.d.ts",
            "lib.dom.d.ts",
            "lib.dom.iterable.d.ts",
            "lib.es5.d.ts",
            "lib.es6.d.ts",
            "lib.es2015.collection.d.ts",
            "lib.es2015.core.d.ts",
            "lib.es2015.d.ts",
            "lib.es2015.generator.d.ts",
            "lib.es2015.iterable.d.ts",
            "lib.es2015.promise.d.ts",
            "lib.es2015.proxy.d.ts",
            "lib.es2015.reflect.d.ts",
            "lib.es2015.symbol.d.ts",
            "lib.es2015.symbol.wellknown.d.ts",
            "lib.es2016.array.include.d.ts",
            "lib.es2016.d.ts",
            "lib.es2016.full.d.ts",
            "lib.es2016.intl.d.ts",
            "lib.es2017.arraybuffer.d.ts",
            "lib.es2017.d.ts",
            "lib.es2017.date.d.ts",
            "lib.es2017.full.d.ts",
            "lib.es2017.intl.d.ts",
            "lib.es2017.object.d.ts",
            "lib.es2017.sharedmemory.d.ts",
            "lib.es2017.string.d.ts",
            "lib.es2017.typedarrays.d.ts",
            "lib.es2018.asyncgenerator.d.ts",
            "lib.es2018.asynciterable.d.ts",
            "lib.es2018.d.ts",
            "lib.es2018.full.d.ts",
            "lib.es2018.intl.d.ts",
            "lib.es2018.promise.d.ts",
            "lib.es2018.regexp.d.ts",
            "lib.es2019.array.d.ts",
            "lib.es2019.d.ts",
            "lib.es2019.full.d.ts",
            "lib.es2019.intl.d.ts",
            "lib.es2019.object.d.ts",
            "lib.es2019.string.d.ts",
            "lib.es2019.symbol.d.ts",
            "lib.es2020.bigint.d.ts",
            "lib.es2020.d.ts",
            "lib.es2020.date.d.ts",
            "lib.es2020.full.d.ts",
            "lib.es2020.intl.d.ts",
            "lib.es2020.number.d.ts",
            "lib.es2020.promise.d.ts",
            "lib.es2020.sharedmemory.d.ts",
            "lib.es2020.string.d.ts",
            "lib.es2020.symbol.wellknown.d.ts",
            "lib.es2021.d.ts",
            "lib.es2021.full.d.ts",
            "lib.es2021.intl.d.ts",
            "lib.es2021.promise.d.ts",
            "lib.es2021.string.d.ts",
            "lib.es2021.weakref.d.ts",
            "lib.es2022.array.d.ts",
            "lib.es2022.d.ts",
            "lib.es2022.error.d.ts",
            "lib.es2022.full.d.ts",
            "lib.es2022.intl.d.ts",
            "lib.es2022.object.d.ts",
            "lib.es2022.regexp.d.ts",
            "lib.es2022.string.d.ts",
            "lib.es2023.array.d.ts",
            "lib.es2023.collection.d.ts",
            "lib.es2023.d.ts",
            "lib.es2023.full.d.ts",
            "lib.es2023.intl.d.ts",
            "lib.es2024.arraybuffer.d.ts",
            "lib.es2024.collection.d.ts",
            "lib.es2024.d.ts",
            "lib.es2024.full.d.ts",
            "lib.es2024.object.d.ts",
            "lib.es2024.promise.d.ts",
            "lib.es2024.regexp.d.ts",
            "lib.es2024.sharedmemory.d.ts",
            "lib.es2024.string.d.ts",
            "lib.esnext.array.d.ts",
            "lib.esnext.collection.d.ts",
            "lib.esnext.d.ts",
            "lib.esnext.decorators.d.ts",
            "lib.esnext.disposable.d.ts",
            "lib.esnext.float16.d.ts",
            "lib.esnext.full.d.ts",
            "lib.esnext.intl.d.ts",
            "lib.esnext.iterator.d.ts",
            "lib.esnext.promise.d.ts",
            "lib.scripthost.d.ts",
            "lib.webworker.asynciterable.d.ts",
            "lib.webworker.d.ts",
            "lib.webworker.importscripts.d.ts",
            "lib.webworker.iterable.d.ts",
        );
    }
}

pub fn eval_with_fs(
    root: PathBuf,
    tsconfig: &NormalizedTsConfig,
    default_lib_dir: PathBuf,
    default_libs: Vec<PathBuf>,
    mut fs: impl CachedFileSystem,
    mut atoms: bolt_ts_atom::AtomIntern,
) -> Output {
    bolt_ts_tracing::init_tracing();
    // ==== collect entires ====
    let config_file_specs = cli::ConfigFileSpecs::get_config_file_specs(tsconfig);
    let mut include = get_filenames(&config_file_specs, &root, &mut fs, &mut atoms);
    include.sort();
    include.dedup();

    let cap = (default_libs.len() + include.len()) * 2;
    let mut module_arena = ModuleArena::new(cap * 8);

    let entries_with_read_file = default_libs
        .into_iter()
        .map(|lib| (lib, true))
        .chain(include.into_iter().map(|inc| (inc, false)))
        .collect::<Vec<_>>()
        .into_par_iter()
        .map(|(p, is_default_lib)| {
            debug_assert!(p.is_normalized());
            if cfg!(target_arch = "wasm32") {
                (None, p, is_default_lib)
            } else {
                let Ok(content) = read_file_with_encoding(&p) else {
                    panic!("failed to read file: {p:?}");
                };
                (Some(content), p, is_default_lib)
            }
        })
        .collect::<Vec<_>>();

    let default_lib_filename = bolt_ts_libs::get_default_lib_filename(tsconfig.compiler_options());

    let entries = entries_with_read_file
        .into_iter()
        .filter_map(|(content, p, is_default_lib)| {
            if is_default_lib && p.file_name().unwrap() == "test.d.ts" {
                let content = content.unwrap();
                let computed_atom = atoms.atom(&content);
                let atom = fs.add_file(&p, content, Some(computed_atom), &mut atoms);
                assert_eq!(computed_atom, atom);
                return Some(module_arena.new_module_with_content(p, is_default_lib, atom, &atoms));
            }
            if is_default_lib && p.file_name().unwrap() != default_lib_filename {
                return None;
            }
            let m = if cfg!(target_arch = "wasm32") {
                assert!(content.is_none());
                module_arena.new_module(p, is_default_lib, &mut fs, &mut atoms)
            } else {
                let content = content.unwrap();
                let computed_atom = atoms.atom(&content);
                let atom = fs.add_file(&p, content, Some(computed_atom), &mut atoms);
                assert_eq!(computed_atom, atom);
                module_arena.new_module_with_content(p, is_default_lib, atom, &atoms)
            };
            Some(m)
        })
        .collect::<Vec<_>>();

    // ==== build graph ====
    let mut p = bolt_ts_parser::ParsedMap::new();
    let atoms = Arc::new(Mutex::new(atoms));
    let herd = bolt_ts_arena::bumpalo_herd::Herd::new();
    let mut mg = bolt_ts_module_graph::build_graph(
        &mut module_arena,
        &entries,
        atoms.clone(),
        &default_lib_dir,
        &herd,
        &mut p,
        fs,
    );

    // ==== bind ====
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();

    let (bind_list, mut p) = {
        let (bind_list, p_map): (Vec<BinderResult<'_>>, Vec<ParseResultForGraph<'_>>) =
            bind_parallel(module_arena.modules(), &atoms, p, tsconfig)
                .into_iter()
                .unzip();
        let p = ParsedMap::from_map(p_map);
        (bind_list, p)
    };

    let diags = p
        .steal_errors()
        .into_iter()
        .chain(mg.steal_errors())
        .collect::<Vec<_>>();

    let MergeGlobalSymbolResult {
        mut bind_list,
        merged_symbols,
        global_symbols,
    } = bolt_ts_binder::merge_global_symbol(&p, &atoms, bind_list, &module_arena);

    let (flow_nodes, flow_in_nodes) = bind_list
        .iter_mut()
        .map(|x| {
            (
                std::mem::take(&mut x.flow_nodes),
                std::mem::take(&mut x.flow_in_nodes),
            )
        })
        .collect::<(Vec<_>, Vec<_>)>();

    // ==== name resolution ====
    let early_resolve_result = early_resolve_parallel(
        module_arena.modules(),
        &bind_list,
        &p,
        &global_symbols,
        &merged_symbols,
        &atoms,
    );

    let states = module_arena
        .modules()
        .iter()
        .zip(early_resolve_result)
        .zip(bind_list)
        .map(|((x, y), z)| (x, y, z))
        .map(|(_, early_resolve_result, mut state)| {
            debug_assert!(
                state
                    .final_res
                    .keys()
                    .all(|node_id| !early_resolve_result.final_res.contains_key(node_id))
            );
            state.diags.extend(early_resolve_result.diags);
            state.final_res.extend(early_resolve_result.final_res);
            ResolveResult {
                symbols: state.symbols,
                final_res: state.final_res,
                diags: state.diags,
                locals: state.locals,
                parent_map: state.parent_map,
            }
        })
        .collect::<Vec<_>>();

    let mut binder = Binder::new(states);

    let diags: Vec<_> = diags
        .into_iter()
        .chain(binder.steal_errors())
        .chain(well_formed_check_parallel(
            &p,
            &atoms,
            module_arena.modules(),
            tsconfig.compiler_options(),
            &binder.bind_results,
        ))
        .collect();

    // ==== type check ====
    let ty_arena = bolt_ts_arena::bumpalo::Bump::with_capacity(1024 * 1024);
    let merged_res = bolt_ts_checker::check::merge_module_augmentation_list_for_global(
        &p,
        &atoms,
        binder.bind_results,
        &module_arena,
        global_symbols,
        merged_symbols,
    );
    let mut binder = bolt_ts_binder::Binder::new(merged_res.bind_list);
    let mut merged_symbols = merged_res.merged_symbols;
    let mut global_symbols = merged_res.global_symbols;
    let mut checker = bolt_ts_checker::check::TyChecker::new(
        &ty_arena,
        &p,
        &mg,
        atoms,
        tsconfig.compiler_options(),
        flow_nodes,
        flow_in_nodes,
        &module_arena,
        &mut binder,
        &mut merged_symbols,
        &mut global_symbols,
    );
    for item in &entries {
        let is_default_lib = module_arena.get_module(*item).is_default_lib();
        let prev = checker.diags.len();
        let root = p.root(*item);
        checker.check_program(root);
        checker.check_deferred_nodes(*item);
        if p.get(*item).is_external_or_commonjs_module() {
            checker.check_external_module_exports(root);
        }
        assert!(
            !is_default_lib || prev == checker.diags.len(),
            "default lib({:#?}) has error",
            module_arena.get_path(*item)
        );
    }

    let diags = diags
        .into_iter()
        .chain(std::mem::take(&mut checker.diags))
        .collect::<Vec<_>>();
    let diags = diag::get_merged_diags(diags, &p, &module_arena);

    let types_len = checker.ty_len();

    let dts_output = if tsconfig
        .compiler_options()
        .flags()
        .contains(CompilerOptionFlags::DECLARATION)
    {
        emit_declaration_parallel(&entries, &checker)
    } else {
        entries
            .iter()
            .filter_map(|&item| {
                let is_default_lib = module_arena.get_module(item).is_default_lib();
                if is_default_lib {
                    None
                } else {
                    Some((item, "".to_string()))
                }
            })
            .collect()
    };
    let js_output = optimize_and_js_emit(entries, checker);

    let ir = js_output.ir;
    let mut files = vec![];
    for ((item, js_code), (item2, dts_code)) in js_output.files.into_iter().zip(dts_output) {
        debug_assert!(
            item == item2,
            "id in js output: {item:#?}, id in dts output: {item2:#?}"
        );
        let output_file = OutputFile { js_code, dts_code };
        files.push((item, output_file));
    }

    debug_assert!({
        // each module should be created at most once
        let paths = module_arena
            .modules()
            .iter()
            .map(|m| module_arena.get_path(m.id()))
            .collect::<Vec<_>>();
        debug_assert!(paths.iter().all(|p| p.is_normalized()));
        paths.len() == paths.iter().collect::<std::collections::HashSet<_>>().len()
    });

    Output {
        root,
        module_arena,
        diags,
        files,
        ir,
        types_len,
    }
}
