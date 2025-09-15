use bolt_ts_checker::check::errors::DeclKind;
use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

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

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type alias name cannot be '{name}'.")]
pub(super) struct TypeAliasNameCannotBeX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Debug)]
pub(super) enum AmbientContextKind {
    Initializers,
    Statements,
}

impl std::fmt::Display for AmbientContextKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            AmbientContextKind::Initializers => "Initializers",
            AmbientContextKind::Statements => "Statements",
        };
        write!(f, "{s}")
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{kind} are not allowed in ambient contexts.")]
pub(super) struct XAreNotAllowedInAmbientContexts {
    #[label(primary)]
    pub span: Span,
    pub kind: AmbientContextKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The body of an 'if' statement cannot be the empty statement.")]
pub(super) struct TheBodyOfAnIfStatementCannotBeTheEmptyStatement {
    #[label(primary)]
    pub span: Span,
}
