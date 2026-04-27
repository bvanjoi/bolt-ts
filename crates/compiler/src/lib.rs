mod cli;
mod compiler_result;
mod diag;
mod emit_declaration;
mod match_files;
mod wf;

use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use self::cli::get_filenames;
pub use self::compiler_result::{CompilerResult, OutputFile};
use self::emit_declaration::emit_declarations;
use self::wf::well_formed_check_parallel;

use bolt_ts_atom::AtomIntern;
use bolt_ts_binder::bind_parallel;
use bolt_ts_binder::{Binder, ResolveResult};
use bolt_ts_binder::{BinderResult, MergeGlobalSymbolResult};
use bolt_ts_checker::check::MergeModuleAugmentationResult;
use bolt_ts_config::{CompilerOptionFlags, NormalizedTsConfig, parse_tsconfig};
use bolt_ts_early_resolve::early_resolve_parallel;
use bolt_ts_fs::{CachedFileSystem, MemoryFS, read_file_with_encoding};
use bolt_ts_optimize::optimize_and_js_emit;
use bolt_ts_parser::{ParseResultForGraph, ParsedMap};
use bolt_ts_span::ModuleArena;
use bolt_ts_utils::path::NormalizePath;

use rayon::prelude::*;

pub const DEFAULT_TSCONFIG: &str = "tsconfig.json";

pub fn current_exe_dir() -> std::path::PathBuf {
    std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

pub fn init_atom() -> AtomIntern {
    #[cfg(debug_assertions)]
    for idx in 0..bolt_ts_ast::keyword::KEYWORDS.len() {
        let t = bolt_ts_ast::keyword_idx_to_token(idx);
        if t == bolt_ts_ast::TokenKind::Var {
            assert_eq!(t as u8 + 1, bolt_ts_ast::TokenKind::Let as u8);
            assert_eq!(t as u8 + 2, bolt_ts_ast::TokenKind::Const as u8);
        }
    }
    bolt_ts_ast::keyword::init_atom_map()
}

pub fn add_default_libs_into_memory_files(
    files: &mut indexmap::IndexMap<String, String>,
    default_lib_dir: &str,
) {
    macro_rules! add_default_lib_file {
        ($($name: literal),* $( , )?) => {
            $(
                let code = include_str!(concat!("../../libs/src/declared_file/", $name));
                let path = format!("{default_lib_dir}/{}", $name);
                let prev = files.insert(path, code.to_string());
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

pub fn eval_from_memory_path_worker<'cx>(
    cwd: String,
    default_lib_dir: String,
    parser_arena: &'cx bolt_ts_arena::bumpalo_herd::Herd,
    type_arena: &'cx bolt_ts_arena::bumpalo::Bump,
    mut fs: MemoryFS,
    mut atoms: AtomIntern,
) -> CompilerResult<'cx, MemoryFS> {
    let default_lib_dir = std::path::PathBuf::from(default_lib_dir);
    let tsconfig: bolt_ts_config::RawTsConfig = if let Ok(raw_tsconfig) =
        fs.read_file(std::path::Path::new("/tsconfig.json"), &mut atoms)
    {
        parse_tsconfig(atoms.get(raw_tsconfig)).unwrap()
    } else {
        Default::default()
    };
    let default_libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| default_lib_dir.join(filename))
        .collect::<Vec<_>>();
    let tsconfig = tsconfig.normalize();
    let root = std::path::PathBuf::from(cwd);
    eval_with_fs(
        root,
        tsconfig,
        default_lib_dir,
        default_libs,
        parser_arena,
        type_arena,
        fs,
        atoms,
    )
}

pub fn eval_from_memory_path<'cx>(
    cwd: String,
    default_lib_dir: String,
    parser_arena: &'cx bolt_ts_arena::bumpalo_herd::Herd,
    type_arena: &'cx bolt_ts_arena::bumpalo::Bump,
    mut files: indexmap::IndexMap<String, String>,
) -> CompilerResult<'cx, MemoryFS> {
    let mut atoms = self::init_atom();
    add_default_libs_into_memory_files(&mut files, &default_lib_dir);
    let fs = bolt_ts_fs::MemoryFS::new(files.into_iter(), &mut atoms).unwrap();
    eval_from_memory_path_worker(cwd, default_lib_dir, parser_arena, type_arena, fs, atoms)
}

pub fn eval_with_fs<'cx, FS: CachedFileSystem>(
    root: PathBuf,
    tsconfig: NormalizedTsConfig,
    default_lib_dir: PathBuf,
    default_libs: Vec<PathBuf>,
    parser_arena: &'cx bolt_ts_arena::bumpalo_herd::Herd,
    type_arena: &'cx bolt_ts_arena::bumpalo::Bump,
    mut fs: FS,
    mut atoms: bolt_ts_atom::AtomIntern,
) -> CompilerResult<'cx, FS> {
    bolt_ts_tracing::init_tracing();
    // ==== collect entires ====
    let config_file_specs = cli::ConfigFileSpecs::get_config_file_specs(&tsconfig);
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
            let content = if fs.is_vfs() {
                None
            } else {
                let Ok(content) = read_file_with_encoding(&p) else {
                    panic!("failed to read file: {p:?}");
                };
                Some(content)
            };
            (content, p, is_default_lib)
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
            let m = if fs.is_vfs() {
                assert!(content.is_none());
                module_arena.new_module(
                    p,
                    is_default_lib,
                    |p, atoms| {
                        let Ok(atom) = fs.read_file(p, atoms) else {
                            return None;
                        };
                        Some(atom)
                    },
                    &mut atoms,
                )
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
    let fs = Arc::new(Mutex::new(fs));
    let mut mg = bolt_ts_module_graph::build_graph(
        &mut module_arena,
        &entries,
        atoms.clone(),
        &default_lib_dir,
        parser_arena,
        &mut p,
        fs.clone(),
        &tsconfig,
    );
    let fs = Arc::try_unwrap(fs).unwrap().into_inner().unwrap();

    // ==== bind ====
    let atoms = Arc::try_unwrap(atoms).unwrap();
    let atoms = atoms.into_inner().unwrap();

    let (bind_list, mut p) = {
        let (bind_list, p_map): (Vec<BinderResult<'_>>, Vec<ParseResultForGraph<'_>>) =
            bind_parallel(&module_arena, &atoms, p, &tsconfig)
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
        bind_list,
        merged_symbols,
        global_symbols,
    } = bolt_ts_binder::merge_global_symbol(&p, &atoms, bind_list, &module_arena);
    let MergeModuleAugmentationResult {
        mut bind_list,
        merged_symbols,
        global_symbols,
    } = bolt_ts_checker::check::merge_module_augmentation_list_for_global(
        &p,
        &atoms,
        bind_list,
        &module_arena,
        global_symbols,
        merged_symbols,
    );

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
    let options = tsconfig.compiler_options();
    let emit_standard_class_fields = options
        .flags()
        .contains(CompilerOptionFlags::USE_DEFINE_FOR_CLASS_FIELDS)
        && *options.target() >= bolt_ts_config::Target::ES2022;
    let early_resolve_result = early_resolve_parallel(
        module_arena.modules(),
        &bind_list,
        &p,
        &global_symbols,
        &merged_symbols,
        &atoms,
        emit_standard_class_fields,
        options,
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
    let binder = bolt_ts_binder::Binder::new(binder.bind_results);
    let mut checker = bolt_ts_checker::check::TyChecker::new(
        type_arena,
        p,
        mg,
        atoms,
        tsconfig,
        flow_nodes,
        flow_in_nodes,
        module_arena,
        binder,
        merged_symbols,
        global_symbols,
    );
    for item in &entries {
        let is_default_lib = checker.module_arena.get_module(*item).is_default_lib();
        let prev = checker.diags.len();
        let root = checker.p.root(*item);
        checker.check_program(root);
        checker.check_deferred_nodes(*item);
        if checker.p.get(*item).is_external_or_commonjs_module() {
            checker.check_external_module_exports(root);
        }
        assert!(
            !is_default_lib || prev == checker.diags.len(),
            "default lib({:#?}) has error",
            checker.module_arena.get_path(*item)
        );
    }

    let diags = diags
        .into_iter()
        .chain(std::mem::take(&mut checker.diags))
        .collect::<Vec<_>>();
    let diags = diag::get_merged_diags(diags, &checker.p, &checker.module_arena);

    debug_assert!({
        // each module should be created at most once
        let paths = checker
            .module_arena
            .modules()
            .iter()
            .map(|m| checker.module_arena.get_path(m.id()))
            .collect::<Vec<_>>();
        debug_assert!(paths.iter().all(|p| p.is_normalized()));
        paths.len() == paths.iter().collect::<std::collections::HashSet<_>>().len()
    });

    let compiler_options = checker.config.compiler_options().flags();
    let mut files = vec![];
    if !compiler_options.contains(CompilerOptionFlags::NO_EMIT) {
        let dts_output = if compiler_options.contains(CompilerOptionFlags::DECLARATION) {
            emit_declarations(&entries, &mut checker)
        } else {
            entries
                .iter()
                .filter_map(|&item| {
                    let is_default_lib = checker.module_arena.get_module(item).is_default_lib();
                    if is_default_lib {
                        None
                    } else {
                        Some((item, "".to_string()))
                    }
                })
                .collect()
        };
        let js_output = optimize_and_js_emit(entries, &mut checker);

        for ((item, js_code), (item2, dts_code)) in js_output.files.into_iter().zip(dts_output) {
            debug_assert!(
                item == item2,
                "id in js output: {item:#?}, id in dts output: {item2:#?}"
            );
            let output_file = OutputFile::new(js_code, dts_code);
            files.push((item, output_file));
        }
    }
    CompilerResult::new(root, diags, fs, files, checker)
}
