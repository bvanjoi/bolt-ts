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
    let libs = bolt_ts_libs::LIBS
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
    let es5 = check_for_test_default_lib_file_count(ES5);
    expect_test::expect!["3"].assert_eq(&es5.to_string());
    let es2015 = check_for_test_default_lib_file_count(ES2015);
    expect_test::expect!["19"].assert_eq(&es2015.to_string());
    let es2016 = check_for_test_default_lib_file_count(ES2016);
    expect_test::expect!["22"].assert_eq(&es2016.to_string());
    let es2017 = check_for_test_default_lib_file_count(ES2017);
    expect_test::expect!["30"].assert_eq(&es2017.to_string());
    let es2018 = check_for_test_default_lib_file_count(ES2018);
    expect_test::expect!["36"].assert_eq(&es2018.to_string());
    let es2019 = check_for_test_default_lib_file_count(ES2019);
    expect_test::expect!["42"].assert_eq(&es2019.to_string());
    let es2020 = check_for_test_default_lib_file_count(ES2020);
    expect_test::expect!["51"].assert_eq(&es2020.to_string());
    let es2021 = check_for_test_default_lib_file_count(ES2021);
    expect_test::expect!["56"].assert_eq(&es2021.to_string());
    let es2022 = check_for_test_default_lib_file_count(ES2022);
    expect_test::expect!["63"].assert_eq(&es2022.to_string());
    let es2023 = check_for_test_default_lib_file_count(ES2023);
    expect_test::expect!["67"].assert_eq(&es2023.to_string());
    let es2024 = check_for_test_default_lib_file_count(ES2024);
    expect_test::expect!["75"].assert_eq(&es2024.to_string());
    let es2025 = check_for_test_default_lib_file_count(ES2025);
    expect_test::expect!["82"].assert_eq(&es2025.to_string());
    let esnext = check_for_test_default_lib_file_count(ESNext);
    expect_test::expect!["93"].assert_eq(&esnext.to_string());
}
