use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Duplicate identifier '{name}'.")]
pub(crate) struct DuplicateIdentifier {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    #[label("Previous definition here")]
    pub original_span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Identifier expected. '{ident}' is a reserved word in strict mode. Class definitions are automatically in strict mode."
)]
pub(crate) struct IdentifierExpected0IsAReservedWordInStrictModeClassDefinitionsAreAutomaticallyInStrictMode
{
    #[label(primary)]
    pub span: Span,
    pub ident: String,
}
