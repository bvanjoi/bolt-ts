use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
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

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error(
    "Module '\"{module_name}\"' declares '{symbol_name}' locally, but it is exported as '{target_name}'."
)]
pub(super) struct ModuleADeclaresBLocallyButItIsExportedAsC {
    #[label(primary)]
    pub span: Span,
    pub module_name: String,
    pub symbol_name: String,
    pub target_name: String,
    #[related]
    pub related: Vec<ModuleADeclaresBLocallyButItIsExportedAsCHelperKind>,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
pub enum ModuleADeclaresBLocallyButItIsExportedAsCHelperKind {
    #[error(transparent)]
    #[diagnostic(transparent)]
    NameIsDeclaredHere(NameIsDeclaredHere),
    #[error(transparent)]
    #[diagnostic(transparent)]
    ExportedAliasHere(ExportedAliasHere),
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{name}' has been alias here.")]
#[diagnostic(severity(Advice))]
pub(super) struct ExportedAliasHere {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}
