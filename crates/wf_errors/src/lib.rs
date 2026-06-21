use bolt_ts_checker_errors::DeclKind;
use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_span::Span;

use thiserror;
use thiserror::Error;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{kind} name cannot be '{name}'.")]
#[diagnostic(help("Reserved type names are not permitted."))]
pub struct DeclNameCannotBe {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    pub kind: DeclKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Abstract methods can only appear within an abstract class.")]
pub struct AbstractMethodsCanOnlyAppearWithinAnAbstractClass {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Catch clause variable cannot have an initializer.")]
pub struct CatchClauseVariableTypeAnnotationMustBeAnyOrUnknownIfSpecified {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Duplicate identifier 'arguments'. Compiler uses 'arguments' to initialize rest parameters."
)]
pub struct DuplicateIdentifierArgumentsCompilerUsesArgumentsToInitializeRestParameters {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type alias name cannot be '{name}'.")]
pub struct TypeAliasNameCannotBeX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Debug)]
pub enum AmbientContextKind {
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
pub struct XAreNotAllowedInAmbientContexts {
    #[label(primary)]
    pub span: Span,
    pub kind: AmbientContextKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The body of an 'if' statement cannot be the empty statement.")]
pub struct TheBodyOfAnIfStatementCannotBeTheEmptyStatement {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Enum name cannot be '{name}'.")]
pub struct EnumNameCannotBeX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An implementation cannot be declared in ambient contexts.")]
pub struct AnImplementationCannotBeDeclaredInAmbientContexts {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Initializer cannot be declared in ambient contexts.")]
pub struct InitializersAreNotAllowedInAmbientContexts {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("A parameter initializer is only allowed in a function or constructor implementation.")]
pub struct AParameterInitializerIsOnlyAllowedInAFunctionOrConstructorImplementation {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Parameter cannot have question mark and initializer.")]
pub struct ParameterCannotHaveQuestionMarkAndInitializer {
    #[label(primary)]
    pub span: Span,
}
