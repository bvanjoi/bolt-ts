use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_errors::DiagnosticExt;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Module '\"{module_name}\"' declares '{symbol_name}' locally, but it is not exported.")]
pub(super) struct ModuleADeclaresBLocallyButItIsNotExported {
    #[label(primary)]
    pub span: Span,
    pub module_name: String,
    pub symbol_name: String,
    #[related]
    pub related: [NameIsDeclaredHere; 1],
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{name}' is defined here")]
#[diagnostic(severity(Advice))]
pub(super) struct NameIsDeclaredHere {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}
