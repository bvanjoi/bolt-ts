use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_errors::DiagnosticExt;
use bolt_ts_span::Span;

fn url(path: &str) -> String {
    const PREFIX: &str = "https://tc39.es/ecma262/";
    format!("{PREFIX}#{path}")
}

/// # 13.10.1: Runtime Semantics: Evaluation
///
/// https://tc39.es/ecma262/#sec-relational-operators-runtime-semantics-evaluation
#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The right value of the `in` operator must be an 'object', but got '{ty}'.")]
#[diagnostic(url("{}", url("sec-relational-operators-runtime-semantics-evaluation")))]
pub(crate) struct TheRightValueOfTheInOperatorMustBeAnObjectButGotTy {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

/// # 15.1 Parameter Lists
///
/// https://tc39.es/ecma262/#prod-FormalParameters
#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A rest parameter must be last in a parameter list.")]
#[diagnostic(url("{}", url("prod-FormalParameters")))]
pub(crate) struct ARestParameterMustBeLastInAParameterList {
    #[label(primary)]
    pub span: Span,
}
