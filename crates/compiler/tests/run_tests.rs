use bolt_ts_compiler::eval_from;
use bolt_ts_errors::miette::Severity;
use compile_test::run_tests::run;
use compile_test::{ensure_node_exist, run_node};

#[test]
fn run_tests() {
    ensure_node_exist();
    let project_root = project_root::get_project_root().unwrap();
    let sub = "tests/cases/compiler";
    let cases = compile_test::fixtures(&project_root, sub);
    let runner = |case: &std::path::Path| {
        let output = eval_from(bolt_ts_span::ModulePath::Real(case.to_path_buf()));
        if output.diags.is_empty() {
            if output.output.trim().is_empty() {
                return Ok(());
            }
            let file_path = compile_test::temp_node_file(
                &project_root,
                case.file_stem().unwrap().to_str().unwrap(),
            );
            std::fs::write(file_path.as_path(), &output.output).unwrap();
            let expected_js_file_path = expect_test::expect_file![case.with_extension("js")];
            expected_js_file_path.assert_eq(&output.output);
            match run_node(&file_path) {
                Ok(Some(output)) => {
                    let expected_file_path = expect_test::expect_file![case.with_extension("out")];
                    expected_file_path.assert_eq(&output);
                }
                Ok(None) => {}
                Err(_) => return Err(vec![]),
            }
            Ok(())
        } else {
            let errors = output
                .diags
                .iter()
                .flat_map(|diag| {
                    let Some(labels) = diag.inner.labels() else {
                        return vec![];
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

                    let mut count = 0;
                    let mut has_primary = false;
                    let mut errors = vec![];
                    for label in labels {
                        count += 1;
                        if label.primary() {
                            has_primary = true;
                        }
                        let msg = if let Some(msg) = label.label() {
                            msg.to_string()
                        } else {
                            msg.clone()
                        };
                        let code = output.module_arena.content_map.get(&diag.module_id);
                        let start = code.map(|code| {
                            bolt_ts_errors::miette_label_span_to_line_position(label, code).0
                        });
                        errors.push(compile_test::errors::Error {
                            line_num: start.map_or(1, |x| x.line + 1),
                            kind: Some(kind),
                            msg,
                        })
                    }
                    if count > 0 {
                        assert!(has_primary)
                    }
                    errors
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
    for case in cases.into_iter() {
        assert!(case.file_type().unwrap().is_dir());
        let case = case.path().join("index.ts");
        run(case.as_path(), runner);
    }
}
