use rts_errors::miette;
use rts_errors::miette::Diagnostic;
use rts_errors::thiserror;
use rts_errors::thiserror::Error;
use rts_span::Span;

#[derive(Error, Diagnostic, Debug)]
#[error("Operator '{op}' cannot be applied to types '{ty1}' and '{ty2}'.")]
pub(super) struct OperatorCannotBeAppliedToTy1AndTy2 {
    #[label]
    pub span: Span,
    pub op: String,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("The value '{value}' cannot be used here.")]
pub(super) struct TheValueCannotBeUsedHere {
    #[label]
    pub span: Span,
    pub value: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Type '{ty1}' is not assignable to type '{ty2}'.")]
pub(super) struct TypeIsNotAssignableToType {
    #[label]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot find name '{name}'.")]
pub(super) struct CannotFindName {
    #[label]
    pub span: Span,
    pub name: String,
    #[related]
    pub errors: Vec<crate::Diag>,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Did you mean the static member '{name}'?")]
#[diagnostic(severity(Advice))]
pub(super) struct DidYourMeanTheStaticMember {
    #[label]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Argument of type '{arg_ty}' is not assignable to parameter of type '{param_ty}'.")]
pub(super) struct ArgumentOfTyIsNotAssignableToParameterOfTy {
    #[label]
    pub span: Span,
    pub arg_ty: String,
    pub param_ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Expected {x} arguments, but got {y}.")]
pub(super) struct ExpectedXArgsButGotY {
    #[label]
    pub span: Span,
    pub x: crate::check::ExpectedArgsCount,
    pub y: usize,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Expected at least {x} arguments, but got {y}.")]
pub(super) struct ExpectedAtLeastXArgsButGotY {
    #[label]
    pub span: Span,
    pub x: usize,
    pub y: usize,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot assign to '{name}' because it is a {ty}.")]
pub(super) struct CannotAssignToNameBecauseItIsATy {
    #[label]
    pub span: Span,
    pub name: String,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("The '{op1}' operator is not allowed for boolean types. Consider using '{op2}' instead.")]
pub(super) struct TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
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
pub(super) struct TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
    #[label]
    pub span: Span,
    pub left_or_right: LeftOrRight,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Object literal may only specify known properties, and '{field}' does not exist.")]
pub(super) struct ObjectLitMayOnlySpecifyKnownPropAndFieldDoesNotExist {
    #[label]
    pub span: Span,
    pub field: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Property '{field}' is missing.")]
pub(super) struct PropertyXIsMissing {
    #[label]
    pub span: Span,
    pub field: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Cannot create an instance of an abstract class.")]
pub(super) struct CannotCreateAnInstanceOfAnAbstractClass {
    #[label]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[error("A class cannot implement a primitive type like '{ty}'. It can only implement other named object types.")]
pub(super) struct AClassCannotImplementAPrimTy {
    #[label]
    pub span: Span,
    pub ty: String,
}

#[derive(Clone, Copy, Debug)]
pub(super) enum DeclKind {
    Class,
    Enum,
}

impl DeclKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Class => "Class",
            Self::Enum => "Enum",
        }
    }
}

impl std::fmt::Display for DeclKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Error, Diagnostic, Debug)]
#[error("{kind} '{name}' used before its declaration.")]
pub(super) struct CannotUsedBeforeItsDeclaration {
    #[label]
    pub span: Span,
    pub kind: DeclKind,
    pub name: String,
    #[related]
    pub related: [DefinedHere; 1],
}

#[derive(Error, Diagnostic, Debug)]
#[error("{kind} '{name}' is defined here")]
#[diagnostic(severity(Advice))]
pub(super) struct DefinedHere {
    #[label]
    pub span: Span,
    pub kind: DeclKind,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Constructor implementation is missing.")]
pub(super) struct ConstructorImplementationIsMissing {
    #[label]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug)]
#[error("Function implementation is missing or not immediately following the declaration")]
pub(super) struct FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration {
    #[label]
    pub span: Span,
}
