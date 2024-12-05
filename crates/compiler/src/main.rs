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
    use bolt_ts_compiler::eval_from;
    let start = std::time::Instant::now();
    let p = get_absolute_path(input_path);
    let output = eval_from(bolt_ts_span::ModulePath::Real(p.clone()));
    output
        .diags
        .into_iter()
        .for_each(|diag| diag.emit(&output.module_arena));
    let duration = start.elapsed();
    dbg!(duration);
}

#[test]
fn main_test() {
    use bolt_ts_compiler::eval_from;
    let project_root = project_root::get_project_root().unwrap();

    // // for hang debug
    // let sub = "tests/cases/compiler";
    // let cases = compile_test::fixtures(&project_root, sub);
    // for case in cases.into_iter() {
    //     let case = case.path().join("index.ts");
    //     dbg!("eval {}", case.display());
    //     println!("eval {}", case.display());
    //     let p = case.to_string_lossy().to_string();
    //     let s = p.as_str();
    //     dbg!(s);
    //     let output = eval_from(bolt_ts_span::ModulePath::Real(case));
    //     dbg!(s);
    // }

    let p = project_root.join("tests/cases/compiler/indexer/index.ts");
    let output = eval_from(bolt_ts_span::ModulePath::Real(p.clone()));
    if output.diags.is_empty() {
        let file_path =
            compile_test::temp_node_file(&project_root, p.file_stem().unwrap().to_str().unwrap());
        dbg!(file_path.as_path());
        std::fs::write(file_path.as_path(), output.output).unwrap();
        compile_test::ensure_node_exist();
        compile_test::run_node(&file_path).unwrap();
    } else {
        output
            .diags
            .into_iter()
            .for_each(|diag| diag.emit(&output.module_arena));
    }
}
