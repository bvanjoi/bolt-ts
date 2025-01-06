use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_errors::DiagnosticExt;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot find name '{name}'.")]
pub(super) struct CannotFindName {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    #[related]
    pub errors: Vec<CannotFindNameHelperKind>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]

pub(super) enum CannotFindNameHelperKind {
    #[error(transparent)]
    DidYouMeanTheStaticMember(DidYourMeanTheStaticMember),
    #[error(transparent)]
    AnInterfaceCannotExtendAPrimTy(AnInterfaceCannotExtendAPrimTy),
    #[error(transparent)]
    AClassCannotImplementAPrimTy(AClassCannotImplementAPrimTy),
    #[error(transparent)]
    CannotUseNamespaceAsTyOrValue(CannotUseNamespaceAsTyOrValue),
}

impl CannotFindNameHelperKind {
    pub fn into_diag(
        self,
    ) -> Box<dyn bolt_ts_errors::diag_ext::DiagnosticExt + Send + Sync + 'static> {
        match self {
            CannotFindNameHelperKind::DidYouMeanTheStaticMember(diag) => Box::new(diag),
            CannotFindNameHelperKind::AnInterfaceCannotExtendAPrimTy(diag) => Box::new(diag),
            CannotFindNameHelperKind::AClassCannotImplementAPrimTy(diag) => Box::new(diag),
            CannotFindNameHelperKind::CannotUseNamespaceAsTyOrValue(diag) => Box::new(diag),
        }
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Did you mean the static member '{name}'?")]
#[diagnostic(severity(Advice))]
pub(super) struct DidYourMeanTheStaticMember {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A class cannot implement a primitive type like '{ty}'.")]
#[diagnostic(help = "It can only implement other named object types.")]
pub(super) struct AClassCannotImplementAPrimTy {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An interface cannot extend a primitive type like '{ty}'.")]
#[diagnostic(help = "It can only extend other named object types.")]
pub(super) struct AnInterfaceCannotExtendAPrimTy {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot use namespace as a {}", if *is_ty { "type" } else { "value" })]
#[diagnostic(severity(Advice))]
pub(super) struct CannotUseNamespaceAsTyOrValue {
    #[label(primary)]
    pub span: Span,
    pub is_ty: bool,
}
