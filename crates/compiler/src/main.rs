fn main() {}

#[test]
fn main_test() {
    use rts_compiler::eval_from;
    let project_root = project_root::get_project_root().unwrap();

    // for hang debug
    // let sub = "tests/cases/compiler";
    // let cases = compile_test::fixtures(&project_root, sub);
    // for case in cases.into_iter() {
    //     if case.path().extension().map_or(false, |ext| ext == "stderr") {
    //         continue;
    //     }
    //     dbg!("eval {}", case.path().display());
    //     println!("eval {}", case.path().display());
    //     let p = case.path().to_string_lossy().to_string();
    //     let s = p.as_str();
    //     dbg!(s);
    //     let output = eval_from(rts_span::ModulePath::Real(case.path().clone()));
    // }

    let p = project_root.join("tests/cases/compiler/classHeritageWithTrailingSeparator.ts");
    let output = eval_from(rts_span::ModulePath::Real(p.clone()));
    if output.diags.is_empty() {
        let file_path =
            compile_test::temp_node_file(&project_root, p.file_stem().unwrap().to_str().unwrap());
        dbg!(file_path.as_path());
        std::fs::write(file_path.as_path(), output.output).unwrap();
        compile_test::ensure_node_exist();
        compile_test::run_node(&file_path);
    } else {
        output
            .diags
            .into_iter()
            .for_each(|diag| diag.emit(&output.module_arena));
    }
}
