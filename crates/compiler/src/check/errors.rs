use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
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
#[error("Expected {x} type arguments, but got {y}.")]
pub(super) struct ExpectedXTyArgsButGotY {
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
#[error(
    "The {left_or_right}-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type."
)]
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
    #[related]
    pub related: [DefinedHere; 1],
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Type '{ty1}' is missing the following properties from type '{ty2}': {}, and {len} more.", {
        props.join(", ")
    }
)]
pub(super) struct Type0IsMissingTheFollowingPropertiesFromType1Colon2And3More {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
    pub props: Vec<String>,
    pub len: usize,
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
#[error("{} has `abstract` modifier.", {
    if let Some(name) = name {
        format!("Class '{}'", name)
    } else {
        "Class".to_string()
    }
})]
#[diagnostic(severity(Warning))]
pub(super) struct ClassNameHasAbstractModifier {
    #[label(primary)]
    pub span: Span,
    pub name: Option<String>,
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) enum DeclKind {
    #[default]
    Class,
    Interface,
    Enum,
    Property,
    BlockScopedVariable,
}

impl DeclKind {
    fn as_str(&self) -> &'static str {
        match self {
            DeclKind::Class => "Class",
            DeclKind::Enum => "Enum",
            DeclKind::Property => "Property",
            DeclKind::Interface => "Interface",
            DeclKind::BlockScopedVariable => "Block-scoped variable",
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

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Property '{prop}' does not exist on type '{ty}'.")]
pub(super) struct PropertyXDoesNotExistOnTypeY {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub ty: String,
    #[related]
    pub related: Vec<PropertyXDoesNotExistOnTypeYHelperKind>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
pub(super) enum PropertyXDoesNotExistOnTypeYHelperKind {
    #[error(transparent)]
    #[diagnostic(transparent)]
    DidYourMeanToAccessTheStaticMemberInstead(DidYourMeanToAccessTheStaticMemberInstead),
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Did you mean to access the static member '{class_name}.{prop_name}' instead?")]
#[diagnostic(severity(Advice))]
pub(super) struct DidYourMeanToAccessTheStaticMemberInstead {
    #[label(primary)]
    pub span: Span,
    pub class_name: String,
    pub prop_name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Generic type '{ty}' requires {n} type argument{}.", { if *n > 1 { "s" } else { "" } })]
pub(super) struct GenericTypeXRequiresNTypeArguments {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub n: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Generic type '{ty}' requires between {x} and {y} type arguments.")]
pub(super) struct GenericTypeXRequiresBetweenXAndYTypeArguments {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub x: usize,
    pub y: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{x}' recursively references itself as a base type.")]
pub(super) struct TypeXRecursivelyReferencesItselfAsABaseType {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    #[label = "Base type is defined here."]
    pub base_defined_span: Option<Span>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{ty}' cannot be used to index type '{index_ty}'.")]
pub(super) struct TypeCannotBeUsedToIndexType {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub index_ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{ty}' cannot be used as an index type.")]
pub(super) struct TypeCannotBeUsedAsAnIndexType {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type."
)]
pub(super) struct AnIndexSignatureParameterTypeMustBeStringNumberSymbolOrATemplateLiteralType {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "A class can only implement an object type or intersection of object types with statically known members."
)]
pub(super) struct AClassCanOnlyImplementAnObjectTypeOrIntersectionOfObjectTypesWithStaticallyKnownMembers
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A class cannot implement a primitive type like '{ty}'.")]
#[diagnostic(help = "It can only implement other named object types.")]
pub(super) struct AClassCannotImplementAPrimTy {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("No overload matches this call.")]
pub(super) struct NoOverloadMatchesThisCall {
    #[label(primary)]
    pub span: Span,
    #[label(collection, "Unmatched call.")]
    pub unmatched_calls: Vec<Span>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("This overload signature is not compatible with its implementation signature.")]
pub(super) struct ThisOverloadSignatureIsNotCompatibleWithItsImplementationSignature {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("This expression is not {}.", if *is_call {
    "callable"
} else {
    "constructable"
})]
pub(super) struct ThisExpressionIsNotConstructable {
    #[label(primary)]
    pub span: Span,
    pub is_call: bool,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type parameter '{ty}' has a circular constraint.")]
pub(super) struct TypeParameterXHasACircularConstraint {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot assign to '{prop}' because it is a read-only property.")]
pub(super) struct CannotAssignTo0BecauseItIsAReadOnlyProperty {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "The right-hand side of a 'for...in' statement must be of type 'any', an object type or a type parameter, but here has type '{ty}'."
)]
pub(super) struct TheRightHandSideOfAForInStatementMustBeOfTypeAnyAnObjectTypeOrATypeParameterButHereHasType0
{
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type produces a tuple type that is too large to represent.")]
pub(super) struct TypeProducesATupleTypeThatIsTooLargeToRepresent {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Interface '{derived}' incorrectly extends interface '{base}'.")]
pub(super) struct Interface0IncorrectlyExtendsInterface1 {
    #[label(primary)]
    pub span: Span,
    pub base: String,
    pub derived: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Ambient module declaration cannot specify relative module name.")]
pub(super) struct AmbientModuleDeclarationCannotSpecifyRelativeModuleName {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A rest parameter must be of an array type.")]
pub(super) struct ARestParameterMustBeOfAnArrayType {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'this' cannot be referenced in a module or namespace body.")]
pub(super) struct ThisCannotBeReferencedInAModuleOrNamespaceBody {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Module '\"{module_name}\"' declares '{symbol_name}' locally, but it is not exported.")]
pub(super) struct ModuleADeclaresBLocallyButItIsNotExported {
    #[label(primary)]
    pub span: Span,
    pub module_name: String,
    pub symbol_name: String,
    #[related]
    pub related: [NameIsDeclaredHere; 1],
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{name}' is defined here")]
#[diagnostic(severity(Advice))]
pub(super) struct NameIsDeclaredHere {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error(
    "Module '\"{module_name}\"' declares '{symbol_name}' locally, but it is exported as '{target_name}'."
)]
pub(super) struct ModuleADeclaresBLocallyButItIsExportedAsC {
    #[label(primary)]
    pub span: Span,
    pub module_name: String,
    pub symbol_name: String,
    pub target_name: String,
    #[related]
    pub related: Vec<ModuleADeclaresBLocallyButItIsExportedAsCHelperKind>,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
pub(super) enum ModuleADeclaresBLocallyButItIsExportedAsCHelperKind {
    #[error(transparent)]
    #[diagnostic(transparent)]
    NameIsDeclaredHere(NameIsDeclaredHere),
    #[error(transparent)]
    #[diagnostic(transparent)]
    ExportedAliasHere(ExportedAliasHere),
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{name}' has been alias here.")]
#[diagnostic(severity(Advice))]
pub(super) struct ExportedAliasHere {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Module '{module}' has no exported member '{member}'.")]
pub(super) struct ModuleXHasNoExportedMemberY {
    #[label(primary)]
    pub span: Span,
    pub module: String,
    pub member: String,
}

#[derive(Debug, Default)]
pub(super) enum UndefinedOrNull {
    Undefined,
    Null,
    #[default]
    Both,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{name}' is possibly {}.", {
    match kind {
        UndefinedOrNull::Both => "undefined or null",
        UndefinedOrNull::Undefined => "undefined",
        UndefinedOrNull::Null => "null",
    }
})]
pub(super) struct XIsPossiblyNullOrUndefined {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    pub kind: UndefinedOrNull,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' is not a constructor function type.")]
pub(super) struct TypeXIsNotAConstructorFunctionType {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "The operand of an {} operator must be a variable or a property access.", {
        if *is_incr {
            "increment"
        } else {
            "decrement"
        }
    }
)]
pub(super) struct TheOperandOfAnIncrementOrDecrementOperatorMustBeAVariableOrAPropertyAccess {
    #[label(primary)]
    pub span: Span,
    pub is_incr: bool,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "The operand of an {} operator may not be an optional property access.", {
        if *is_incr {
            "increment"
        } else {
            "decrement"
        }
    }
)]
pub(super) struct TheOperandOfAnIncrementOrDecrementOperatorMayNotBeAnOptionalPropertyAccess {
    #[label(primary)]
    pub span: Span,
    pub is_incr: bool,
}
