use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};
use std::path::Path;

use super::CompilerOptions;
use super::common::{FailMode, PassMode};
use super::header::parse_compiler_options;

pub(super) struct TestPropsBuilder;

#[derive(Default, Debug, Clone)]
pub struct TestPropsAttrs {
    compiler_options: CompilerOptions,
    skip_message_match: bool,
    pass_mode: Option<PassMode>,
    fail_mode: Option<FailMode>,
}

impl TestPropsAttrs {
    pub fn skip_message_match(&self) -> bool {
        self.skip_message_match
    }

    pub fn pass_mode(&self) -> Option<PassMode> {
        self.pass_mode
    }

    pub fn fail_mode(&self) -> Option<FailMode> {
        self.fail_mode
    }

    pub fn compiler_options(&self) -> &CompilerOptions {
        &self.compiler_options
    }

    fn update(&mut self, ln: &str) {
        let Some(directive) = parse_directive(ln) else {
            return;
        };
        match directive {
            Directive::CompilerOptions(options) => {
                let parsed = parse_compiler_options(options);
                self.compiler_options.extend(parsed);
            }
            Directive::PassMode(pass_mode) => {
                if self.pass_mode.is_some() {
                    panic!("multiple `*-pass` headers in a single test");
                }
                self.pass_mode = Some(pass_mode);
            }
            Directive::FailMode(fail_mode) => {
                if self.fail_mode.is_some() {
                    panic!("multiple `*-fail` headers in a single test");
                }
                self.fail_mode = Some(fail_mode);
            }
            Directive::SkipMessageMatch => {
                if self.skip_message_match {
                    panic!("multiple `skip-message-match` headers in a single test");
                }
                self.skip_message_match = true;
            }
        }
    }
}

enum Directive<'a> {
    CompilerOptions(&'a str),
    PassMode(PassMode),
    FailMode(FailMode),
    SkipMessageMatch,
}

fn parse_directive<'a>(line: &'a str) -> Option<Directive<'a>> {
    let (key, value) = line
        .split_once(':')
        .map(|(k, v)| (k.trim(), v.trim()))
        .unwrap_or((line, ""));
    if key == "compiler-options" {
        assert!(!value.is_empty());
        return Some(Directive::CompilerOptions(value));
    }
    let ret = |d: Directive<'a>| -> Option<Directive<'a>> {
        assert!(value.is_empty());
        Some(d)
    };
    match key {
        "check-pass" => ret(Directive::PassMode(PassMode::Check)),
        "build-pass" => ret(Directive::PassMode(PassMode::Build)),
        "run-pass" => ret(Directive::PassMode(PassMode::Run)),
        "check-fail" => ret(Directive::FailMode(FailMode::Check)),
        "build-fail" => ret(Directive::FailMode(FailMode::Build)),
        "run-fail" => ret(Directive::FailMode(FailMode::Run)),
        "skip-message-match" => ret(Directive::SkipMessageMatch),
        _ => None,
    }
}

fn build_test_props(s: &str) -> TestProps {
    let mut base_attr = TestPropsAttrs::default();
    let rdr = std::io::Cursor::new(s);
    iter_header(rdr.clone(), &mut |HeaderLine {
                                       directive,
                                       header_revision,
                                       ..
                                   }| {
        match header_revision {
            Some(_) => {}
            None => {
                base_attr.update(directive);
            }
        }
    });
    let mut map = HashMap::new();
    iter_header(rdr.clone(), &mut |HeaderLine {
                                       header_revision,
                                       directive,
                                       ..
                                   }| {
        let Some(revision) = header_revision else {
            return;
        };
        let entry = map
            .entry(TestPropsKey::Custom(revision.to_string()))
            .or_insert_with(|| base_attr.clone());
        entry.update(directive);
    });

    if map.is_empty() {
        TestProps::Single(base_attr)
    } else {
        TestProps::Multiple(map)
    }
}

impl TestPropsBuilder {
    pub fn from_file(test_file: &Path) -> TestProps {
        let file = File::open(test_file).unwrap();
        let s = std::io::read_to_string(file).unwrap();
        build_test_props(&s)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TestPropsKey {
    Base,
    Custom(String),
}

#[derive(Debug)]
pub enum TestProps {
    Single(TestPropsAttrs),
    Multiple(HashMap<TestPropsKey, TestPropsAttrs>),
}

const COMMENT: &str = "//@";

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
    /// Some header directives start with a revision name in square brackets
    /// (e.g. `[foo]`), and only apply to that revision of the test.
    /// If present, this field contains the revision name (e.g. `foo`).
    header_revision: Option<&'ln str>,
    /// The main part of the header directive, after removing the comment prefix
    /// and the optional revision specifier.
    directive: &'ln str,
}

fn iter_header(rdr: impl Read, iter: &mut dyn FnMut(HeaderLine<'_>)) {
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
        if let Some(d) = line_directive(COMMENT, ln) {
            // iter(HeaderLine { line_number, original_line, header_revision, directive });
            iter(HeaderLine {
                header_revision: d.line_revision,
                directive: d.directive,
            });
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
struct LineDirective<'line> {
    line_revision: Option<&'line str>,
    directive: &'line str,
}

/// Extract an `(Option<line_revision>, directive)` directive from a line if comment is present.
///
fn line_directive<'line>(comment: &str, original_line: &'line str) -> Option<LineDirective<'line>> {
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

        Some(LineDirective {
            line_revision: Some(line_revision),
            directive: directive.trim_start(),
        })
    } else {
        Some(LineDirective {
            line_revision: None,
            directive: after_comment,
        })
    }
}

#[test]
fn test_line_directive() {
    #[track_caller]
    fn t(ln: &str, expected: LineDirective<'static>) {
        assert_eq!(line_directive(COMMENT, ln), Some(expected));
    }
    fn assert_none(ln: &str) {
        assert_eq!(line_directive(COMMENT, ln), None);
    }
    fn d(s: &'static str) -> LineDirective<'static> {
        LineDirective {
            line_revision: None,
            directive: s,
        }
    }
    assert_none("//run-pass");
    assert_none("// run-pass");
    t(
        "//@[foo]",
        LineDirective {
            line_revision: Some("foo"),
            directive: "",
        },
    );
    t(
        "//@[foo] check-pass",
        LineDirective {
            line_revision: Some("foo"),
            directive: "check-pass",
        },
    );
    t(
        "//@[foo] check-pass ",
        LineDirective {
            line_revision: Some("foo"),
            directive: "check-pass ",
        },
    );
    t("//@ run-pass", d("run-pass"));
    t("//@check-pass", d("check-pass"));
    t("//@ check-pass", d("check-pass"));
    t("//@   check-pass", d("check-pass"));
    t("//@   check-pass  ", d("check-pass  "));
    t("//@😊check-pass", d("😊check-pass"));
    t("//@ compiler-options: a=b", d("compiler-options: a=b"));
    t("//@ compiler-options: a=b,d", d("compiler-options: a=b,d"));
    t(
        "//@ compiler-options: a=b, d",
        d("compiler-options: a=b, d"),
    );
}

#[test]
fn test_single_config_options() {
    use serde_json::json;
    let build_single_options = |s: &str| {
        let props = build_test_props(s);
        match props {
            TestProps::Single(attrs) => attrs.compiler_options().clone(),
            TestProps::Multiple(_) => panic!("expected single test props"),
        }
    };

    let header = r#"
//@compiler-options: a1=b1
//@compiler-options: a2=b2 a3=b3
//@compiler-options: a4=true a5=false
//@compiler-options: a6
//@compiler-options: a7.
//@compiler-options: a8=[foo,bar]
//@compiler-options: a9=[a.b, c2]
"#
    .trim();
    let o = build_single_options(header);
    assert_eq!(o["a1"], json!("b1"));
    assert_eq!(o["a2"], json!("b2"));
    assert_eq!(o["a3"], json!("b3"));
    assert_eq!(o["a4"], json!(true));
    assert_eq!(o["a5"], json!(false));
    assert_eq!(o["a6"], json!(true));
    assert_eq!(o["a8"], json!(["foo", "bar"]));
    assert_eq!(o["a9"], json!(["a.b", "c2"]));
}

#[cfg(test)]
fn build_multiple_options(s: &str) -> HashMap<TestPropsKey, TestPropsAttrs> {
    let props = build_test_props(s);
    match props {
        TestProps::Multiple(map) => map,
        TestProps::Single(_) => panic!("expected multiple test props"),
    }
}

#[test]
fn test_multiple_config_options_0() {
    use serde_json::json;
    let header = r#"
//@compiler-options: a1=b1
//@[c1]compiler-options: a2=b2
//@[c2] compiler-options: a2=b3
"#
    .trim();
    let map = build_multiple_options(header);

    assert_eq!(map.len(), 2);

    // c1
    let key = TestPropsKey::Custom("c1".to_string());
    let o = map.get(&key).unwrap().compiler_options();
    assert_eq!(o.len(), 2);
    assert_eq!(o["a1"], json!("b1"));
    assert_eq!(o["a2"], json!("b2"));

    // c2
    let key = TestPropsKey::Custom("c2".to_string());
    let o = map.get(&key).unwrap().compiler_options();
    assert_eq!(o.len(), 2);
    assert_eq!(o["a1"], json!("b1"));
    assert_eq!(o["a2"], json!("b3"));
}

#[test]
fn test_props_attrs_should_not_panic() {
    let mut attrs = TestPropsAttrs::default();
    attrs.update("ts-expect-error");
    attrs.update("ts-expect-error:123");
}

#[test]
#[should_panic]
fn test_props_attrs_should_panic_if_has_useless_value() {
    let mut attrs = TestPropsAttrs::default();
    attrs.update("run-fail:123");
}

#[test]
#[should_panic]
fn test_props_attrs_should_panic_if_lack_useful_value() {
    let mut attrs = TestPropsAttrs::default();
    attrs.update("compiler-options");
}
