use bolt_ts_compiler::eval_from;
use bolt_ts_config::RawTsConfig;
use bolt_ts_errors::miette::Severity;
use compile_test::run_tests::run;
use compile_test::{ensure_node_exist, run_node};

#[test]
fn ensure_node_exist_in_current_env() {
    ensure_node_exist();
    assert_eq!(1 + 1, 2);
}

#[test]
fn ensure_all_cases_are_dir() {
    let case_dir = project_root::get_project_root()
        .unwrap()
        .join("tests/cases/compiler");
    for case in std::fs::read_dir(case_dir).unwrap() {
        let case = case.unwrap();
        assert!(case.path().is_dir(), "{:?} is not a directory", case.path());
    }
}

#[dir_test::dir_test(
    dir: "$CARGO_MANIFEST_DIR/../../tests/cases/compiler",
    glob: "**/index.ts",
)]
fn run_test(arg: dir_test::Fixture<&str>) {
    let entry = std::path::Path::new(arg.path());

    let runner = |case: &std::path::Path| {
        let dir = case.parent().unwrap();
        let tsconfig_file = dir.join("tsconfig.json");
        let tsconfig = if tsconfig_file.is_file() {
            let s = std::fs::read_to_string(tsconfig_file).unwrap();
            serde_json::from_str(&s).unwrap()
        } else {
            RawTsConfig::default().with_include(vec!["index.ts".to_string()])
        };
        let output = eval_from(dir.to_path_buf(), tsconfig.normalize());
        if output.diags.is_empty() {
            let mut file_paths = vec![];
            for (m, contents) in &output.output {
                if contents.trim().is_empty() {
                    continue;
                }
                let file_path = match output.module_arena.get_path(*m) {
                    bolt_ts_span::ModulePath::Real(p) => p,
                    bolt_ts_span::ModulePath::Virtual => todo!(),
                };

                let temp_node_file =
                    compile_test::temp_node_file(file_path.file_stem().unwrap().to_str().unwrap());
                std::fs::write(temp_node_file.as_path(), &contents).unwrap();
                file_paths.push(temp_node_file);

                let expected_js_file_path =
                    expect_test::expect_file![file_path.with_extension("js")];
                expected_js_file_path.assert_eq(&contents);
            }

            if file_paths.len() == 1 {
                let temp_file_path = file_paths.pop().unwrap();
                match run_node(&temp_file_path) {
                    Ok(Some(output)) => {
                        let expected_file_path =
                            expect_test::expect_file![case.with_extension("out")];
                        expected_file_path.assert_eq(&output);
                    }
                    Ok(None) => {}
                    Err(_) => return Err(vec![]),
                }
            }
            Ok(())
        } else {
            let errors = output
                .diags
                .iter()
                .filter_map(|diag| {
                    let Some(labels) = diag.inner.labels() else {
                        return None;
                    };

                    let msg = diag.inner.to_string();
                    let kind = diag.inner.severity().map_or(
                        compile_test::errors::ErrorKind::Error,
                        |severity| match severity {
                            Severity::Advice => compile_test::errors::ErrorKind::Help,
                            Severity::Warning => compile_test::errors::ErrorKind::Warning,
                            Severity::Error => compile_test::errors::ErrorKind::Error,
                        },
                    );

                    let primary_label = labels
                        .into_iter()
                        .find(|label| label.primary())
                        .expect("at least one primary label");
                    let msg = if let Some(msg) = primary_label.label() {
                        msg.to_string()
                    } else {
                        msg.clone()
                    };
                    let code = output.module_arena.get_content(diag.module_id);
                    let start =
                        bolt_ts_errors::miette_label_span_to_line_position(primary_label, code).0;
                    Some(compile_test::errors::Error {
                        line_num: start.line + 1,
                        kind: Some(kind),
                        msg,
                    })
                })
                .collect::<Vec<compile_test::errors::Error>>();
            let expected_file_path = expect_test::expect_file![case.with_extension("stderr")];
            let err_msg = output
                .diags
                .into_iter()
                .map(|diag| diag.emit_message(&output.module_arena, true))
                .collect::<Vec<_>>()
                .join("\n");
            expected_file_path.assert_eq(&err_msg);
            Err(errors)
        }
    };

    run(entry, runner);
}
