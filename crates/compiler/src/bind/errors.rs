use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, Debug)]
#[error("Duplicate identifier '{name}'.")]
pub(super) struct DuplicateIdentifier {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    #[label("Previous definition here")]
    pub original_span: Span,
}
