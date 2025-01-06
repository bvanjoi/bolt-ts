use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_errors::DiagnosticExt;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Operator '{op}' cannot be applied to types '{ty1}' and '{ty2}'.")]
pub(super) struct OperatorCannotBeAppliedToTy1AndTy2 {
    #[label(primary)]
    pub span: Span,
    pub op: String,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The value '{value}' cannot be used here.")]
pub(super) struct TheValueCannotBeUsedHere {
    #[label(primary)]
    pub span: Span,
    pub value: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{ty1}' is not assignable to type '{ty2}'.")]
pub(super) struct TypeIsNotAssignableToType {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Argument of type '{arg_ty}' is not assignable to parameter of type '{param_ty}'.")]
pub(super) struct ArgumentOfTyIsNotAssignableToParameterOfTy {
    #[label(primary)]
    pub span: Span,
    pub arg_ty: String,
    pub param_ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected {x} arguments, but got {y}.")]
pub(super) struct ExpectedXArgsButGotY {
    #[label(primary)]
    pub span: Span,
    pub x: crate::check::ExpectedArgsCount,
    pub y: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected at least {x} arguments, but got {y}.")]
pub(super) struct ExpectedAtLeastXArgsButGotY {
    #[label(primary)]
    pub span: Span,
    pub x: usize,
    pub y: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot assign to '{name}' because it is a {ty}.")]
pub(super) struct CannotAssignToNameBecauseItIsATy {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The '{op1}' operator is not allowed for boolean types. Consider using '{op2}' instead.")]
pub(super) struct TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
    #[label(primary)]
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

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The {left_or_right}-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.")]
pub(super) struct TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
    #[label(primary)]
    pub span: Span,
    pub left_or_right: LeftOrRight,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Object literal may only specify known properties, and '{field}' does not exist.")]
pub(super) struct ObjectLitMayOnlySpecifyKnownPropAndFieldDoesNotExist {
    #[label(primary)]
    pub span: Span,
    pub field: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Property '{field}' is missing.")]
pub(super) struct PropertyXIsMissing {
    #[label(primary)]
    pub span: Span,
    pub field: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot create an instance of an abstract class.")]
pub(super) struct CannotCreateAnInstanceOfAnAbstractClass {
    #[label(primary)]
    pub span: Span,
    #[related]
    pub abstract_class_list: Vec<ClassNameHasAbstractModifier>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Class '{name}' has `abstract` modifier.")]
#[diagnostic(severity(Warning))]
pub(super) struct ClassNameHasAbstractModifier {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Clone, Copy, Debug, Default)]
pub(super) enum DeclKind {
    #[default]
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

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{kind} '{name}' used before its declaration.")]
pub(super) struct CannotUsedBeforeItsDeclaration {
    #[label(primary)]
    pub span: Span,
    pub kind: DeclKind,
    pub name: String,
    #[related]
    pub related: [DefinedHere; 1],
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug, Default)]
#[error("{kind} '{name}' is defined here")]
#[diagnostic(severity(Advice))]
pub(super) struct DefinedHere {
    #[label(primary)]
    pub span: Span,
    pub kind: DeclKind,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Constructor implementation is missing.")]
pub(super) struct ConstructorImplementationIsMissing {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Function implementation is missing or not immediately following the declaration.")]
pub(super) struct FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Value of type '{ty}' is not callable.")]
#[diagnostic(help("Did you mean to include 'new'?"))]
pub(super) struct ValueOfType0IsNotCallable {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Property '{prop}' of type '{ty_b}' is not assignable to '{ty_c}' index type '{index_ty_d}'."
)]
pub(super) struct PropertyAOfTypeBIsNotAssignableToCIndexTypeD {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub ty_b: String,
    pub ty_c: String,
    pub index_ty_d: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{decl}' is referenced directly or indirectly in its own base expression.")]
pub(super) struct DeclIsReferencedDirectlyOrIndirectlyInItsOwnBaseExpression {
    #[label(primary)]
    pub span: Span,
    pub decl: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{ty}' index signatures are incompatible.")]
pub(super) struct IndexSignaturesAreIncompatible {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}
