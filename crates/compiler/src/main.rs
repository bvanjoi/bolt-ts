use bolt_ts_compiler::{current_exe_dir, eval_with_fs, init_atom};
use bolt_ts_config::{RawTsConfig, parse_tsconfig};
use bolt_ts_fs::CachedFileSystem;
use std::env;
use std::path::{Path, PathBuf};

fn get_absolute_path(input_path: &str) -> PathBuf {
    let path = Path::new(input_path);

    if path.is_absolute() {
        path.to_path_buf()
    } else {
        let cwd = env::current_dir().unwrap();
        cwd.join(path)
    }
}

fn main() {
    let start = std::time::Instant::now();
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path>", args[0]);
        std::process::exit(1);
    }

    let input_path = &args[1];
    let p = get_absolute_path(input_path);

    let mut atoms = init_atom();
    let mut fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let tsconfig = if p.ends_with("tsconfig.json") {
        let content = fs.read_file(&p, &mut atoms).unwrap();
        let s = atoms.get(content);
        parse_tsconfig(s).unwrap()
    } else {
        RawTsConfig::default().with_include(vec![p.to_str().unwrap().to_string()])
    };
    let result = run(atoms, fs, tsconfig);
    let duration = start.elapsed();

    println!("Files: {}", result.files);
    println!("Types: {}", result.types);
    println!(
        "Time cost: {}",
        pretty_duration::pretty_duration(&duration, None)
    );
}

struct Result {
    files: usize,
    types: usize,
}

fn run(atoms: bolt_ts_atom::AtomIntern, fs: bolt_ts_fs::LocalFS, tsconfig: RawTsConfig) -> Result {
    let exe_dir = current_exe_dir();
    let libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| exe_dir.join(filename))
        .collect::<Vec<_>>();

    let cwd = env::current_dir().unwrap();
    let tsconfig = tsconfig.normalize();
    let parser_herd = bolt_ts_arena::bumpalo_herd::Herd::new();
    let type_arena = bolt_ts_arena::bumpalo::Bump::new();
    let mut compiler_result = eval_with_fs(
        cwd,
        tsconfig,
        exe_dir,
        libs,
        &parser_herd,
        &type_arena,
        fs,
        atoms,
    );
    let module_arena = compiler_result.steal_module_arena();
    let diags = compiler_result.steal_diags();
    diags.into_iter().for_each(|diag| diag.emit(&module_arena));
    Result {
        files: module_arena.modules().len(),
        types: compiler_result.type_count(),
    }
}

#[cfg(test)]
fn check_for_test_default_lib_file_count(target: bolt_ts_config::RawTarget) -> usize {
    let mut atoms = init_atom();
    let fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let tsconfig = RawTsConfig::default()
        .with_include(vec![])
        .with_compiler_options(bolt_ts_config::RawCompilerOptions::default().with_target(target));
    let result = run(atoms, fs, tsconfig);
    result.files
}

#[test]
fn check_for_test_default_lib_file_count_list() {
    use bolt_ts_config::RawTarget::*;
    // Expected values are the distinct-file counts of each target's default
    // lib closure; they are anchored as literals so a regression in the DAG
    // computation (`bolt_ts_libs::dag::closure_len`) is caught here.
    let es2015 = check_for_test_default_lib_file_count(ES2015);
    assert_eq!(es2015, 19);
    let es5 = check_for_test_default_lib_file_count(ES5);
    assert_eq!(es5, 3);
    let es2016 = check_for_test_default_lib_file_count(ES2016);
    assert_eq!(es2016, 16);
    let es2017 = check_for_test_default_lib_file_count(ES2017);
    assert_eq!(es2017, 24);
    let es2018 = check_for_test_default_lib_file_count(ES2018);
    assert_eq!(es2018, 30);
    let es2019 = check_for_test_default_lib_file_count(ES2019);
    assert_eq!(es2019, 36);
    let es2020 = check_for_test_default_lib_file_count(ES2020);
    assert_eq!(es2020, 45);
    let es2021 = check_for_test_default_lib_file_count(ES2021);
    assert_eq!(es2021, 50);
    let es2022 = check_for_test_default_lib_file_count(ES2022);
    assert_eq!(es2022, 57);
    let es2023 = check_for_test_default_lib_file_count(ES2023);
    assert_eq!(es2023, 61);
    let es2024 = check_for_test_default_lib_file_count(ES2024);
    assert_eq!(es2024, 69);
    let es2025 = check_for_test_default_lib_file_count(ES2025);
    assert_eq!(es2025, 76);
    let esnext = check_for_test_default_lib_file_count(ESNext);
    assert_eq!(esnext, 87);
}
