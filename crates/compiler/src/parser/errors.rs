use rts_errors::miette;
use rts_errors::miette::Diagnostic;
// use rts_errors::thiserror;
use rts_errors::thiserror::Error;
use rts_span::Span;

#[derive(Error, Diagnostic, Debug)]
#[error("Syntax Error: Unexpected token ','")]
pub(crate) struct ClassesCanOnlyExtendASingleClass {
    #[label]
    pub span: Span,
    #[label("Classes can only extend a single class.")]
    pub extra_extends: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type parameter list cannot be empty.")]
pub(crate) struct TypeParameterListCannotBeEmpty {
    #[label]
    pub span: Span,
}
