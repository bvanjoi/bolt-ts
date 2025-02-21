use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
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
    #[diagnostic(transparent)]
    DidYouMeanTheStaticMember(DidYouMeanTheStaticMember),
    #[error(transparent)]
    #[diagnostic(transparent)]
    AnInterfaceCannotExtendAPrimTy(AnInterfaceCannotExtendAPrimTy),
    // #[error(transparent)]
    // #[diagnostic(transparent)]
    // AClassCannotImplementAPrimTy(AClassCannotImplementAPrimTy),
    #[error(transparent)]
    #[diagnostic(transparent)]
    CannotUseNamespaceAsTyOrValue(CannotUseNamespaceAsTyOrValue),
    #[error(transparent)]
    #[diagnostic(transparent)]
    ShorthandPropertyNeedAnInitializer(ShorthandPropertyNeedAnInitializer),
    #[error(transparent)]
    #[diagnostic(transparent)]
    OnlyReferToATypeButIsBeingUsedAsValueHere(OnlyReferToATypeButIsBeingUsedAsValueHere),
}

impl CannotFindNameHelperKind {
    pub fn into_diag(
        self,
    ) -> Box<dyn bolt_ts_errors::diag_ext::DiagnosticExt + Send + Sync + 'static> {
        match self {
            CannotFindNameHelperKind::DidYouMeanTheStaticMember(diag) => Box::new(diag),
            CannotFindNameHelperKind::AnInterfaceCannotExtendAPrimTy(diag) => Box::new(diag),
            // CannotFindNameHelperKind::AClassCannotImplementAPrimTy(diag) => Box::new(diag),
            CannotFindNameHelperKind::CannotUseNamespaceAsTyOrValue(diag) => Box::new(diag),
            CannotFindNameHelperKind::ShorthandPropertyNeedAnInitializer(diag) => Box::new(diag),
            CannotFindNameHelperKind::OnlyReferToATypeButIsBeingUsedAsValueHere(diag) => {
                Box::new(diag)
            }
        }
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{name}' only refers to a type, but is being used as a value here.")]
#[diagnostic(severity(Advice))]
pub(super) struct OnlyReferToATypeButIsBeingUsedAsValueHere {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    #[label("'{name}' is defined here.")]
    pub defined_here: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Did you mean the static member '{class_name}.{prop_name}'?")]
#[diagnostic(severity(Advice))]
pub(super) struct DidYouMeanTheStaticMember {
    #[label(primary)]
    pub span: Span,
    pub class_name: String,
    pub prop_name: String,
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

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Parameter '{name}' cannot reference itself.")]
pub(super) struct ParameterXCannotReferenceItself {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Shorthand property need an initializer.")]
#[diagnostic(severity(Advice))]
pub(super) struct ShorthandPropertyNeedAnInitializer {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Static members cannot reference class type parameters.")]
pub(super) struct StaticMembersCannotReferenceClassTypeParameters {
    #[label(primary)]
    pub span: Span,
}
