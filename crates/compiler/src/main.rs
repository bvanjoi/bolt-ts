use bolt_ts_compiler::eval_from;

fn main() {
    // let start = std::time::Instant::now();
    // let project_root = project_root::get_project_root().unwrap();
    // let p = project_root.join("tests/cases/compiler/fib/index.ts");
    // let output = eval_from(bolt_ts_span::ModulePath::Real(p.clone()));
    // let duration = start.elapsed();
    // dbg!(duration);
}

#[test]
fn main_test() {
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

    let p = project_root.join("tests/cases/compiler/fib/index.ts");
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
