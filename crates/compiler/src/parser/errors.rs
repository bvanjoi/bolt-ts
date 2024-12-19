use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
// use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, Debug)]
#[error("Syntax Error: Unexpected token ','")]
pub(crate) struct ClassesCanOnlyExtendASingleClass {
    #[label(primary)]
    pub span: Span,
    #[label("Classes can only extend a single class.")]
    pub extra_extends: Option<Span>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type parameter list cannot be empty.")]
pub(crate) struct TypeParameterListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Octal literals are not allowed.")]
#[diagnostic(help = "Use the syntax '{help_lit}'.")]
pub(crate) struct OctalLiteralsAreNotAllowed {
    #[label(primary)]
    pub span: Span,
    pub help_lit: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Variable declaration list cannot be empty.")]
pub(crate) struct VariableDeclarationListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}
