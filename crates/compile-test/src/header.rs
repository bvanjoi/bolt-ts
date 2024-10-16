use core::str;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};
use std::path::Path;

use crate::common::{FailMode, PassMode, TestConfig};

fn expand_variables(mut value: String, _config: &TestConfig) -> String {
    const CWD: &str = "{{cwd}}";
    // const SRC_BASE: &str = "{{src-base}}";
    // const BUILD_BASE: &str = "{{build-base}}";

    if value.contains(CWD) {
        let cwd = env::current_dir().unwrap();
        value = value.replace(CWD, &cwd.to_string_lossy());
    }

    value
}

fn parse_name_directive(line: &str, directive: &str) -> bool {
    // Ensure the directive is a whole word.
    line.starts_with(directive)
        && matches!(
            line.as_bytes().get(directive.len()),
            None | Some(&b' ') | Some(&b':')
        )
}

impl TestConfig {
    fn parse_name_directive(&self, line: &str, directive: &str) -> bool {
        parse_name_directive(line, directive)
    }

    fn parse_name_value_directive(&self, line: &str, directive: &str) -> Option<String> {
        let colon = directive.len();
        if line.starts_with(directive) && line.as_bytes().get(colon) == Some(&b':') {
            let value = line[(colon + 1)..].to_owned();
            Some(expand_variables(value, self))
        } else {
            None
        }
    }

    fn update_compiler_options(&mut self, ln: &str) {
        if let Some(opt) = self.parse_name_value_directive(ln, &directives::COMPILER_OPTIONS) {
            for pair in opt.trim().split_whitespace() {
                if let Some((key, value)) = pair.split_once("=") {
                    self.compiler_options
                        .insert(key.to_string(), value.to_string());
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct TestProps {
    pass_mode: Option<PassMode>,
    fail_mode: Option<FailMode>,
}

impl TestProps {
    fn new() -> Self {
        TestProps {
            pass_mode: None,
            fail_mode: None,
        }
    }

    pub fn from_file(test_file: &Path, config: &mut TestConfig) -> Self {
        let mut props = TestProps::new();
        props.load_from(test_file, config);
        props
    }

    fn load_from(&mut self, test_file: &Path, config: &mut TestConfig) {
        if test_file.is_dir() {
            // maybe we need find the entry file
            todo!()
        }
        let file = File::open(test_file).unwrap();
        iter_header(
            test_file,
            file,
            &mut |HeaderLine { directive: ln, .. }| {
                self.update_pass_mode(ln, config);
                self.update_fail_mode(ln, config);
                config.update_compiler_options(ln)
            },
        );
    }

    fn update_pass_mode(&mut self, ln: &str, config: &TestConfig) {
        let pass_mode = if config.parse_name_directive(ln, "check-pass") {
            Some(PassMode::Check)
        } else if config.parse_name_directive(ln, "build-pass") {
            Some(PassMode::Build)
        } else if config.parse_name_directive(ln, "run-pass") {
            Some(PassMode::Run)
        } else {
            None
        };
        match (self.pass_mode, pass_mode) {
            (None, Some(_)) => self.pass_mode = pass_mode,
            (Some(_), Some(_)) => panic!("multiple `*-pass` headers in a single test"),
            (_, None) => {}
        }
    }

    fn update_fail_mode(&mut self, ln: &str, config: &TestConfig) {
        let fail_mode = if config.parse_name_directive(ln, "check-fail") {
            Some(FailMode::Check)
        } else if config.parse_name_directive(ln, "build-fail") {
            Some(FailMode::Build)
        } else if config.parse_name_directive(ln, "run-fail") {
            Some(FailMode::Run)
        } else {
            None
        };
        match (self.fail_mode, fail_mode) {
            (None, Some(_)) => self.fail_mode = fail_mode,
            (Some(_), Some(_)) => panic!("multiple `*-fail` headers in a single test"),
            (_, None) => {}
        }
    }

    pub fn pass_mode(&self) -> Option<PassMode> {
        self.pass_mode
    }

    pub fn fail_mode(&self) -> Option<FailMode> {
        self.fail_mode
    }
}

/// The broken-down contents of a line containing a test header directive,
/// which [`iter_header`] passes to its callback function.
///
/// For example:
///
/// ```text
/// //@ compile-flags: -O
///     ^^^^^^^^^^^^^^^^^ directive
/// ^^^^^^^^^^^^^^^^^^^^^ original_line
///
/// //@ [foo] compile-flags: -O
///      ^^^                    header_revision
///           ^^^^^^^^^^^^^^^^^ directive
/// ^^^^^^^^^^^^^^^^^^^^^^^^^^^ original_line
/// ```
struct HeaderLine<'ln> {
    // line_number: usize,
    // /// Raw line from the test file, including comment prefix and any revision.
    // original_line: &'ln str,
    // /// Some header directives start with a revision name in square brackets
    // /// (e.g. `[foo]`), and only apply to that revision of the test.
    // /// If present, this field contains the revision name (e.g. `foo`).
    // header_revision: Option<&'ln str>,
    /// The main part of the header directive, after removing the comment prefix
    /// and the optional revision specifier.
    directive: &'ln str,
}

const COMMENT: &str = "//@";

fn iter_header(test_file: &Path, rdr: impl Read, iter: &mut dyn FnMut(HeaderLine<'_>)) {
    assert!(test_file.is_file());
    if test_file.extension().is_some_and(|e| e == "ts") {
        // skip
    } else {
        unreachable!()
    };

    let mut rdr = BufReader::with_capacity(1024, rdr);
    // let mut line_number = 0;
    let mut ln = String::new();

    loop {
        // line_number += 1;
        ln.clear();
        if rdr.read_line(&mut ln).unwrap() == 0 {
            break;
        }

        // let original_line = &ln;
        let ln = ln.trim();
        if let Some((_, directive)) = line_directive(COMMENT, ln) {
            // iter(HeaderLine { line_number, original_line, header_revision, directive });
            iter(HeaderLine { directive });
        }
    }
}

/// Extract an `(Option<line_revision>, directive)` directive from a line if comment is present.
///
fn line_directive<'line>(
    comment: &str,
    original_line: &'line str,
) -> Option<(Option<&'line str>, &'line str)> {
    // Ignore lines that don't start with the comment prefix.
    let after_comment = original_line
        .trim_start()
        .strip_prefix(comment)?
        .trim_start();

    if let Some(after_open_bracket) = after_comment.strip_prefix('[') {
        // A comment like `//@[foo]` only applies to revision `foo`.
        let Some((line_revision, directive)) = after_open_bracket.split_once(']') else {
            panic!(
                "malformed condition directive: expected `{comment}[foo]`, found `{original_line}`"
            )
        };

        Some((Some(line_revision), directive.trim_start()))
    } else {
        Some((None, after_comment))
    }
}

mod directives {
    pub const COMPILER_OPTIONS: &'static str = "compiler-options";
}

#[test]
fn test_line_directive() {
    #[track_caller]
    fn t(ln: &str, expected: Option<(Option<&str>, &str)>) {
        assert_eq!(line_directive(COMMENT, ln), expected);
    }
    t("//run-pass", None);
    t("// run-pass", None);
    t("//@[foo]", Some((Some("foo"), "")));
    t("//@[foo] check-pass", Some((Some("foo"), "check-pass")));
    t("//@[foo] check-pass ", Some((Some("foo"), "check-pass ")));
    t("//@ run-pass", Some((None, "run-pass")));
    t("//@check-pass", Some((None, "check-pass")));
    t("//@ check-pass", Some((None, "check-pass")));
    t("//@   check-pass", Some((None, "check-pass")));
    t("//@   check-pass  ", Some((None, "check-pass  ")));
    t("//@😊check-pass", Some((None, "😊check-pass")));
    t(
        "//@ compiler-options: a=b",
        Some((None, "compiler-options: a=b")),
    );
}

#[test]
fn test_parse_name_directive() {
    assert!(parse_name_directive("a", "a"));
    assert!(parse_name_directive("a:", "a"));
    assert!(parse_name_directive("a ", "a"));
    assert!(parse_name_directive("a  ", "a"));
}

#[test]
fn test_config_update_compiler_options() {
    let mut config = TestConfig::new();

    config.update_compiler_options("compiler-options: a1=b1");
    assert_eq!(config.compiler_options.len(), 1);
    assert_eq!(config.compiler_options["a1"], "b1");

    config.update_compiler_options("compiler-options: a2=b2 a3=b3");
    assert_eq!(config.compiler_options.len(), 3);
    assert_eq!(config.compiler_options["a1"], "b1");
    assert_eq!(config.compiler_options["a2"], "b2");
    assert_eq!(config.compiler_options["a3"], "b3");
}
