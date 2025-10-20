use bolt_ts_compiler::output_files;
use bolt_ts_config::{NormalizedTsConfig, RawCompilerOptions, RawTsConfig};
use bolt_ts_errors::miette::Severity;
use bolt_ts_utils::path::NormalizePath;
use compile_test::run_tests::run;
use compile_test::{ensure_node_exist, run_node_with_assert_context};
use std::path::PathBuf;

#[test]
fn ensure_node_exist_in_current_env() {
    ensure_node_exist();
    assert_eq!(1 + 1, 2);
}

#[test]
fn ensure_all_compiler_cases_are_dir() {
    let case_dir = project_root::get_project_root()
        .unwrap()
        .join("tests/compiler");
    for case in std::fs::read_dir(case_dir).unwrap() {
        let case = case.unwrap();
        assert!(case.path().is_dir(), "{:?} is not a directory", case.path());
    }
}

fn eval_in_test(root: PathBuf, tsconfig: &NormalizedTsConfig) -> bolt_ts_compiler::Output {
    // ==== atom init ====
    let mut atoms = bolt_ts_compiler::init_atom();
    // ==== fs init ====
    let fs = bolt_ts_fs::LocalFS::new(&mut atoms);
    let exe_dir = bolt_ts_compiler::current_exe_dir();
    let mut default_libs = bolt_ts_libs::DEFAULT_LIBS
        .iter()
        .map(|filename| exe_dir.join(filename))
        .collect::<Vec<_>>();

    // extra default lib
    let current_dir = std::env::current_dir().unwrap();
    default_libs.push(current_dir.join("tests/test.d.ts"));
    bolt_ts_compiler::eval_with_fs(root, tsconfig, exe_dir, default_libs, fs, atoms)
}

fn run_test(entry: &std::path::Path, try_run_node: bool) {
    const DEFAULT_OUTPUT: &str = "output";

    run(entry, |test_ctx| {
        let file_name = test_ctx.test_file().file_name().unwrap().to_str().unwrap();
        let dir = test_ctx.test_file().parent().unwrap();

        let compiler_options: RawCompilerOptions =
            serde_json::from_value(test_ctx.compiler_options().clone().into()).unwrap();

        debug_assert!(file_name == "index.ts" || file_name == "index.tsx");
        let tsconfig = RawTsConfig::default()
            .with_compiler_options(compiler_options)
            .with_include_if_none(vec![file_name.to_string()])
            .config_compiler_options(|c| {
                c.with_no_emit(true)
                    .with_out_dir(DEFAULT_OUTPUT.to_string())
            });

        let cwd = dir.normalize();
        let tsconfig = tsconfig.normalize();
        let output = eval_in_test(cwd, &tsconfig);
        let output_dir = dir.join(DEFAULT_OUTPUT);
        if !output_dir.exists() {
            std::fs::create_dir(&output_dir).unwrap();
        }
        let output_files =
            output_files(&output.root, &tsconfig, &output.module_arena, output.files);
        let output_err_path = output_dir.join(file_name).with_extension("stderr");
        if output.diags.is_empty() {
            let _ = std::fs::remove_file(output_err_path);

            let mut index_file_path = None;
            for (p, content) in &output_files {
                if content.trim().is_empty() {
                    continue;
                }

                if p.ends_with("index.js") {
                    let temp_node_file =
                        compile_test::temp_node_file(p.file_stem().unwrap().to_str().unwrap());
                    assert!(temp_node_file.is_absolute());
                    assert!(temp_node_file.ends_with("index.js"));
                    if !temp_node_file.exists() {
                        std::fs::create_dir_all(temp_node_file.parent().unwrap()).unwrap();
                    }
                    std::fs::write(temp_node_file.as_path(), content).unwrap();
                    index_file_path = Some(temp_node_file);
                }

                expect_test::expect_file![p].assert_eq(content);
            }

            if let Some(index_file_path) = index_file_path {
                if try_run_node {
                    match run_node_with_assert_context(&index_file_path) {
                        Ok(_) => {}
                        Err(_) => return Err(vec![]),
                    }
                }
            }
            Ok(())
        } else {
            let errors = output
                .diags
                .iter()
                .filter_map(|diag| {
                    let labels = diag.inner.labels()?;
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
                        diag.inner.to_string()
                    };
                    let code = output.module_arena.get_content(diag.inner.module_id());
                    let start =
                        bolt_ts_errors::miette_label_span_to_line_position(primary_label, code).0;
                    Some(compile_test::errors::Error {
                        line_num: start.line + 1,
                        kind: Some(kind),
                        msg,
                    })
                })
                .collect::<Vec<compile_test::errors::Error>>();
            let err_msg = output
                .diags
                .into_iter()
                .map(|diag| diag.emit_message(&output.module_arena, true))
                .collect::<Vec<_>>()
                .join("\n");
            expect_test::expect_file![output_err_path].assert_eq(&err_msg);
            Err(errors)
        }
    });
}

#[dir_test::dir_test(
    dir: "$CARGO_MANIFEST_DIR/../../tests/compiler",
    glob: "**/index.ts",
)]
fn run_index_ts_test(arg: dir_test::Fixture<&str>) {
    let entry = std::path::Path::new(arg.path());
    run_test(entry, true);
}

#[dir_test::dir_test(
    dir: "$CARGO_MANIFEST_DIR/../../tests/compiler",
    glob: "**/index.tsx",
)]
fn run_index_tsx_test(arg: dir_test::Fixture<&str>) {
    let entry = std::path::Path::new(arg.path());
    run_test(entry, false);
}
