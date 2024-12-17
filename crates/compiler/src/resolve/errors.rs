use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot find name '{name}'.")]
pub(super) struct CannotFindName {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    #[related]
    pub errors: Vec<crate::Diag>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Did you mean the static member '{name}'?")]
#[diagnostic(severity(Advice))]
pub(super) struct DidYourMeanTheStaticMember {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("A class cannot implement a primitive type like '{ty}'.")]
#[diagnostic(help = "It can only implement other named object types.")]
pub(super) struct AClassCannotImplementAPrimTy {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("An interface cannot extend a primitive type like '{ty}'.")]
#[diagnostic(help = "It can only extend other named object types.")]
pub(super) struct AnInterfaceCannotExtendAPrimTy {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}
