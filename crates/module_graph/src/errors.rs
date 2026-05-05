use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_span::Span;

use thiserror;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Cannot find module '{module_name}' or its corresponding type declarations.")]
pub(super) struct CannotFindModuleOrItsCorrespondingTypeDeclarations {
    #[label(primary)]
    pub span: Span,
    pub module_name: String,
}
