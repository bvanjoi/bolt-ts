use bolt_ts_compiler::eval_from;
use bolt_ts_config::RawTsConfig;
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
    let tsconfig = RawTsConfig::default().with_include(vec![p.to_str().unwrap().to_string()]);
    let cwd = env::current_dir().unwrap();
    let output = eval_from(cwd, tsconfig.normalize());
    output
        .diags
        .into_iter()
        .for_each(|diag| diag.emit(&output.module_arena));
    let duration = start.elapsed();
    dbg!(duration);
}

#[test]
fn main_test() {
    compile_test::ensure_node_exist();
    let cwd = env::current_dir().unwrap();
    let project_root: PathBuf = project_root::get_project_root().unwrap();
    let p = project_root.join("tests/cases/compiler/propertyAccess3/index.ts");
    let tsconfig = RawTsConfig::default().with_include(vec![p.to_str().unwrap().to_string()]);
    let output = eval_from(cwd, tsconfig.normalize());
    if output.diags.is_empty() {
        let mut file_paths = vec![];
        for (m, contents) in &output.output {
            let p = output.module_arena.get_path(*m);
            let file_path = match p {
                bolt_ts_span::ModulePath::Real(p) => {
                    compile_test::temp_node_file(p.file_stem().unwrap().to_str().unwrap())
                }
                bolt_ts_span::ModulePath::Virtual => todo!(),
            };
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
        if file_paths.len() == 1 {
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
