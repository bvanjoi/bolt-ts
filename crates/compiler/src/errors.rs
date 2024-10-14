use rts_errors::miette;
use rts_errors::miette::Diagnostic;
use rts_errors::thiserror;
use rts_errors::thiserror::Error;
use rts_span::Span;

use crate::check::ExpectedArgsCount;

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

#[derive(Error, Diagnostic, Debug)]
#[error("Type '{ty1}' is not assignable to type '{ty2}'.")]
pub(crate) struct TypeIsNotAssignableToType {
    #[label]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot find name '{name}'.")]
pub(crate) struct CannotFindName {
    #[label]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Argument of type '{arg_ty}' is not assignable to parameter of type '{param_ty}'.")]
pub(crate) struct ArgumentOfTyIsNotAssignableToParameterOfTy {
    #[label]
    pub span: Span,
    pub arg_ty: String,
    pub param_ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Expected {x} arguments, but got {y}.")]
pub(crate) struct ExpectedXArgsButGotY {
    #[label]
    pub span: Span,
    pub x: ExpectedArgsCount,
    pub y: usize,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Expected at least {x} arguments, but got {y}.")]
pub(crate) struct ExpectedAtLeastXArgsButGotY {
    #[label]
    pub span: Span,
    pub x: usize,
    pub y: usize,
}