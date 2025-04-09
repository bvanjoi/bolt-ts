use bolt_ts_compiler::{current_exe_dir, eval_from_with_fs, init_atom};
use bolt_ts_config::RawTsConfig;
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
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <path>", args[0]);
        std::process::exit(1);
    }

    let input_path = &args[1];
    let start = std::time::Instant::now();
    let p = get_absolute_path(input_path);
    let mut atoms = init_atom();
    let mut fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let exe_dir = current_exe_dir();
    let libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| exe_dir.join(filename))
        .collect::<Vec<_>>();
    let tsconfig = if p.ends_with("tsconfig.json") {
        let content = fs.read_file(&p, &mut atoms).unwrap();
        let s = atoms.get(content);
        serde_json::from_str(s).unwrap()
    } else {
        RawTsConfig::default().with_include(vec![p.to_str().unwrap().to_string()])
    };
    let cwd = env::current_dir().unwrap();
    let tsconfig = tsconfig.normalize();
    let output = eval_from_with_fs(cwd, &tsconfig, exe_dir, libs, fs, atoms);
    let duration = start.elapsed();
    output
        .diags
        .into_iter()
        .for_each(|diag| diag.emit(&output.module_arena));
    println!("Files: {}", output.module_arena.modules().len());
    println!("Types: {}", output.types_len);
    println!(
        "Time cost: {}",
        pretty_duration::pretty_duration(&duration, None)
    );
}

#[test]
fn main_test() {
    compile_test::ensure_node_exist();
    let project_root: PathBuf = project_root::get_project_root().unwrap();
    let case_root = project_root.join("tests/cases/compiler/numericUnderscoredSeparator/");
    assert!(case_root.is_dir(), "'{case_root:#?}' not found.",);
    let tsconfig_file = case_root.join(bolt_ts_compiler::DEFAULT_TSCONFIG);
    let tsconfig = if tsconfig_file.is_file() {
        let s = std::fs::read_to_string(tsconfig_file).unwrap();
        serde_json::from_str(&s).unwrap()
    } else {
        RawTsConfig::default()
    };
    let tsconfig = tsconfig.with_include_if_none(vec!["index.ts".to_string()]);
    let tsconfig = tsconfig.normalize();
    let output = bolt_ts_compiler::eval_from(case_root, &tsconfig);
    if output.diags.is_empty() {
        let mut file_paths = vec![];
        for (m, contents) in &output.output {
            let p = output.module_arena.get_path(*m);
            let file_path = compile_test::temp_node_file(p.file_stem().unwrap().to_str().unwrap());
            std::fs::write(file_path.as_path(), contents).unwrap();
            file_paths.push(file_path);
        }

        println!(
            "{:#?}",
            file_paths
                .iter()
                .map(|m| m.as_path().display())
                .collect::<Vec<_>>()
        );
        if output.output.len() == 1 {
            let file_path = file_paths.pop().unwrap();
            compile_test::run_node(&file_path).unwrap();
        }
    } else {
        output
            .diags
            .into_iter()
            .for_each(|diag| diag.emit(&output.module_arena));
    }
}
