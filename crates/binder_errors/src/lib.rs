use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_span::Span;

use thiserror;
use thiserror::Error;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Duplicate identifier '{name}'.")]
pub struct DuplicateIdentifier {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    #[label("Previous definition here")]
    pub original_span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Enum declarations can only merge with namespace or other enum declarations.")]
pub struct EnumDeclarationsCanOnlyMergeWithNamespaceOrOtherEnumDeclarations {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A module cannot have multiple default exports.")]
pub struct AModuleCannotHaveMultipleDefaultExports {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Declaration name conflicts with built-in global identifier '{name}'.")]
pub struct DeclarationNameConflictsWithBuiltInGlobalIdentifierX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}
