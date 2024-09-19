use rts_errors::miette;
use rts_errors::miette::Diagnostic;
use rts_errors::thiserror;
use rts_errors::thiserror::Error;
use rts_span::Span;

#[derive(Error, Diagnostic, Debug)]
#[error("Operator '{op}' cannot be applied to types '{ty1}' and '{ty2}'.")]
pub(crate) struct OperatorCannotBeAppliedToTy1AndTy2 {
    #[label]
    pub span: Span,
    pub op: String,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("The value '{value}' cannot be used here.")]
pub(crate) struct TheValueCannotBeUsedHere {
    #[label]
    pub span: Span,
    pub value: String,
}
