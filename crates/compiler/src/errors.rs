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
    #[related]
    pub errors: Vec<crate::Diag>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Did you mean the static member '{name}'?")]
#[diagnostic(severity(Advice))]
pub(crate) struct DidYourMeanTheStaticMember {
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
    pub x: crate::check::ExpectedArgsCount,
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

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot assign to '{name}' because it is a {ty}.")]
pub(crate) struct CannotAssignToNameBecauseItIsATy {
    #[label]
    pub span: Span,
    pub name: String,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("The '{op1}' operator is not allowed for boolean types. Consider using '{op2}' instead.")]
pub(crate) struct TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
    #[label]
    pub span: Span,
    pub op1: String,
    pub op2: String,
}

#[derive(Clone, Copy, Debug)]
pub(super) enum LeftOrRight {
    Left,
    Right,
}

impl LeftOrRight {
    fn as_str(self) -> &'static str {
        match self {
            Self::Left => "left",
            Self::Right => "right",
        }
    }
}

impl std::fmt::Display for LeftOrRight {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("The {left_or_right}-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.")]
pub(crate) struct TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
    #[label]
    pub span: Span,
    pub left_or_right: LeftOrRight,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Object literal may only specify known properties, and '{field}' does not exist.")]
pub(crate) struct ObjectLitMayOnlySpecifyKnownPropAndFieldDoesNotExist {
    #[label]
    pub span: Span,
    pub field: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Property '{field}' is missing.")]
pub(crate) struct PropertyXIsMissing {
    #[label]
    pub span: Span,
    pub field: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot create an instance of an abstract class.")]
pub(crate) struct CannotCreateAnInstanceOfAnAbstractClass {
    #[label]
    pub span: Span,
}
