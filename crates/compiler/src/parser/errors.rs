use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, Debug)]
#[error("Syntax Error: Unexpected token ','")]
pub(super) struct ClassesCanOnlyExtendASingleClass {
    #[label(primary)]
    pub span: Span,
    #[label("Classes can only extend a single class.")]
    pub extra_extends: Option<Span>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type parameter list cannot be empty.")]
pub(super) struct TypeParameterListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Octal literals are not allowed.")]
#[diagnostic(help = "Use the syntax '{help_lit}'.")]
pub(super) struct OctalLiteralsAreNotAllowed {
    #[label(primary)]
    pub span: Span,
    pub help_lit: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Variable declaration list cannot be empty.")]
pub(super) struct VariableDeclarationListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[error("A parameter property is only allowed in a constructor implementation.")]
pub(super) struct AParamPropIsOnlyAllowedInAConstructorImplementation {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[error("{kind}")]
pub(super) struct MissingIdent {
    #[label(primary)]
    pub span: Span,
    pub kind: MissingIdentKind,
}

#[derive(Error, Diagnostic, Debug)]
pub(super) enum MissingIdentKind {
    #[error("Identifier expected.")]
    IdentifierExpected,
    #[error("Expression expected.")]
    ExpressionExpected,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type argument list cannot be empty.")]
pub(super) struct TypeArgumentListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}
