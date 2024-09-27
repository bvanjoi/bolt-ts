use compile_test::run_tests::run;
use rts_compiler::eval_from;
use rts_errors::miette::Severity;

#[test]
fn run_tests() {
    let project_root = project_root::get_project_root().unwrap();
    let sub = "tests/cases/compiler";
    let cases = compile_test::fixtures(&project_root, sub);
    let runner = |case: &std::path::Path| {
        let (module_arena, diags) = eval_from(rts_span::ModulePath::Real(case.to_path_buf()));
        if diags.is_empty() {
            Ok(())
        } else {
            let errors = diags
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
                    labels
                        .map(|label| {
                            let msg = if let Some(msg) = label.label() {
                                msg.to_string()
                            } else {
                                msg.clone()
                            };
                            let code = module_arena.content_map.get(&diag.module_id);
                            let start = code.map(|code| {
                                rts_errors::miette_label_span_to_line_position(label, unsafe {
                                    core::str::from_utf8_unchecked(code.as_ref())
                                })
                                .0
                            });
                            compile_test::errors::Error {
                                line_num: start.map_or(1, |x| x.line + 1),
                                kind: Some(kind),
                                msg,
                            }
                        })
                        .collect()
                })
                .collect::<Vec<compile_test::errors::Error>>();
            let expected_file_path = expect_test::expect_file![case.with_extension("stderr")];
            let err_msg = diags
                .into_iter()
                .map(|diag| diag.emit_message(&module_arena, true))
                .collect::<Vec<_>>()
                .join("\n");
            expected_file_path.assert_eq(&err_msg);
            Err(errors)
        }
    };
    for case in cases.into_iter() {
        if case.path().extension().map_or(false, |ext| ext == "stderr") {
            continue;
        }
        run(&case.path(), runner);
    }
}
