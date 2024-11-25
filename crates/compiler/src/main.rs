fn main() {
    // let p = std::path::PathBuf::from(
    //     "/Users/bytedance/bolt-ts/tests/cases/compiler/constructorReturnsInvalidType.ts",
    // );
    // let output = bolt_ts_compiler::eval_from(bolt_ts_span::ModulePath::Real(p));
}

#[test]
fn main_test() {
    use bolt_ts_compiler::eval_from;
    let project_root = project_root::get_project_root().unwrap();

    // // for hang debug
    // let sub = "tests/cases/compiler";
    // let cases = compile_test::fixtures(&project_root, sub);
    // for case in cases.into_iter() {
    //     if case.path().extension().map_or(false, |ext| ext != "ts") {
    //         continue;
    //     }
    //     dbg!("eval {}", case.path().display());
    //     println!("eval {}", case.path().display());
    //     let p = case.path().to_string_lossy().to_string();
    //     let s = p.as_str();
    //     dbg!(s);
    //     let output = eval_from(bolt_ts_span::ModulePath::Real(case.path().clone()));
    // }

    let p = project_root.join("tests/cases/compiler/constructorReturnsInvalidType.ts");
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
