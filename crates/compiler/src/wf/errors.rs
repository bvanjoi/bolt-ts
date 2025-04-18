use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

use crate::check::errors::DeclKind;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{kind} name cannot be '{name}'.")]
#[diagnostic(help("Reserved type names are not permitted."))]
pub(super) struct DeclNameCannotBe {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    pub kind: DeclKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Abstract methods can only appear within an abstract class.")]
pub(super) struct AbstractMethodsCanOnlyAppearWithinAnAbstractClass {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An object literal cannot have multiple properties with the same name.")]
pub(super) struct AnObjectLiteralCannotHaveMultiplePropertiesWithTheSameName {
    #[label(primary)]
    pub span: Span,
    #[label("Previous definition here")]
    pub old: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Catch clause variable cannot have an initializer.")]
pub(super) struct CatchClauseVariableTypeAnnotationMustBeAnyOrUnknownIfSpecified {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Duplicate identifier 'arguments'. Compiler uses 'arguments' to initialize rest parameters."
)]
pub(super) struct DuplicateIdentifierArgumentsCompilerUsesArgumentsToInitializeRestParameters {
    #[label(primary)]
    pub span: Span,
}
