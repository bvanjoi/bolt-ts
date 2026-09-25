use std::path::Path;

use super::common::FailMode;
use super::errors;
use super::test_props_builder::TestProps;
use super::test_props_builder::TestPropsAttrs;
use super::test_props_builder::TestPropsBuilder;
use super::test_props_builder::TestPropsKey;

pub fn run(
    test_file: &Path,
    runner: impl FnOnce(RunnerCtx) -> Result<(), Vec<errors::Error>> + Copy,
) {
    let props = TestPropsBuilder::from_file(test_file);
    let cx = TestCx {
        props: &props,
        test_file,
    };
    cx.run_test(runner)
}

#[derive(Copy, Clone)]
struct TestCx<'test> {
    props: &'test TestProps,
    test_file: &'test Path,
}

pub struct RunnerCtx<'test> {
    compiler_options: &'test super::CompilerOptions,
    test_file: &'test Path,
    revision: TestPropsKey,
}

impl RunnerCtx<'_> {
    pub fn compiler_options(&self) -> &super::CompilerOptions {
        self.compiler_options
    }
    pub fn test_file(&self) -> &Path {
        self.test_file
    }
    pub fn revision(&self) -> &TestPropsKey {
        &self.revision
    }
}

impl TestCx<'_> {
    fn run_test(&self, runner: impl FnOnce(RunnerCtx) -> Result<(), Vec<errors::Error>> + Copy) {
        match self.props {
            TestProps::Single(props) => {
                self.run_single_test(runner, &TestPropsKey::Base, props);
            }
            TestProps::Multiple(map) => {
                for (key, props) in map {
                    self.run_single_test(runner, key, props);
                }
            }
        }
    }

    fn run_single_test(
        &self,
        runner: impl FnOnce(RunnerCtx) -> Result<(), Vec<errors::Error>>,
        test_props_key: &TestPropsKey,
        test_props_attrs: &TestPropsAttrs,
    ) {
        let expected_errors = match test_props_key {
            TestPropsKey::Base => errors::load_errors(self.test_file, None),
            TestPropsKey::Custom(revision) => errors::load_errors(self.test_file, Some(revision)),
        };
        let panic = |msg: String| panic!("{msg} in {}", self.test_file.display());
        let ctx = RunnerCtx {
            compiler_options: test_props_attrs.compiler_options(),
            revision: test_props_key.clone(),
            test_file: self.test_file,
        };
        match runner(ctx) {
            Ok(_success) => {
                if !expected_errors.is_empty() {
                    panic("it actually had some expected errors".to_string());
                }
                if test_props_attrs.skip_message_match() {
                    panic!("skip-message-match had been marked but actual run success",);
                }
                assert!(test_props_attrs.fail_mode().is_none());
            }
            Err(errors) => {
                assert!(
                    test_props_attrs.pass_mode().is_none(),
                    r#"
                        ensure you do not mark `//@ {{check, run}}-pass` in header,
                        it should been **failed** but you had been set success flag {pass_mode:#?} in {file}"#,
                    pass_mode = test_props_attrs.pass_mode().unwrap(),
                    file = self.test_file.to_string_lossy()
                );
                if test_props_attrs.skip_message_match() {
                    assert!(
                        expected_errors.is_empty(),
                        "skip-message-match is set, but we still got expected errors in {}",
                        self.test_file.display()
                    );
                    assert!(
                        !errors.is_empty(),
                        "skip-message-match is set, but we cannot find any errors in {}",
                        self.test_file.display()
                    );
                    return;
                }
                if errors.is_empty() {
                    assert_eq!(
                        test_props_attrs.fail_mode(),
                        Some(FailMode::Run),
                        "we cannot find any errors in {} and it not set `//@ run-fail` in header",
                        self.test_file.display()
                    );
                }
                self.check_expected_errors(&expected_errors, &errors);
            }
        }
    }

    fn check_expected_errors(
        &self,
        expected_errors: &[errors::Error],
        actual_errors: &[errors::Error],
    ) {
        let file_name = format!("{}", self.test_file.display()).replace('\\', "/");
        let expect_help = expected_errors
            .iter()
            .any(|ee| ee.kind == Some(errors::ErrorKind::Help));
        let expect_note = expected_errors
            .iter()
            .any(|ee| ee.kind == Some(errors::ErrorKind::Note));

        let mut unexpected = Vec::new();
        let mut found = vec![false; expected_errors.len()];
        for actual_error in actual_errors {
            let opt_index =
                expected_errors
                    .iter()
                    .enumerate()
                    .position(|(index, expected_error)| {
                        !found[index]
                            && actual_error.line_num == expected_error.line_num
                            && (expected_error.kind.is_none()
                                || actual_error.kind == expected_error.kind)
                            && actual_error.msg.contains(&expected_error.msg)
                    });

            match opt_index {
                Some(index) => {
                    // found a match, everybody is happy
                    assert!(!found[index]);
                    found[index] = true;
                }

                None => {
                    // If the test is a known bug, don't require that the error is annotated
                    if self.is_unexpected_compiler_message(actual_error, expect_help, expect_note) {
                        self.error(&format!(
                            "{}:{}: unexpected {}: '{}'",
                            file_name,
                            actual_error.line_num,
                            actual_error
                                .kind
                                .as_ref()
                                .map_or(String::from("message"), |k| k.to_string()),
                            actual_error.msg
                        ));
                        unexpected.push(actual_error);
                    }
                }
            }
        }

        let mut not_found = Vec::new();
        // anything not yet found is a problem
        for (index, expected_error) in expected_errors.iter().enumerate() {
            if !found[index] {
                self.error(&format!(
                    "{}:{}: expected {} not found: {}",
                    file_name,
                    expected_error.line_num,
                    expected_error
                        .kind
                        .as_ref()
                        .map_or("message".into(), |k| k.to_string()),
                    expected_error.msg
                ));
                not_found.push(expected_error);
            }
        }

        if !unexpected.is_empty() || !not_found.is_empty() {
            self.error(&format!(
                "{} unexpected errors found, {} expected errors not found",
                unexpected.len(),
                not_found.len()
            ));
            if !unexpected.is_empty() {
                println!("unexpected errors (from JSON output): {unexpected:#?}\n");
            }
            if !not_found.is_empty() {
                println!("not found errors (from test file): {not_found:#?}\n");
            }
            panic!("test failed for {}", self.test_file.display());
        }
    }

    fn error(&self, err: &str) {
        println!("\nerror: {err}")
    }

    fn is_unexpected_compiler_message(
        &self,
        actual_error: &errors::Error,
        expect_help: bool,
        expect_note: bool,
    ) -> bool {
        !actual_error.msg.is_empty()
            && match actual_error.kind {
                Some(errors::ErrorKind::Help) => expect_help,
                Some(errors::ErrorKind::Note) => expect_note,
                Some(errors::ErrorKind::Error) | Some(errors::ErrorKind::Warning) => true,
                Some(errors::ErrorKind::Suggestion) | None => false,
            }
    }
}
