use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_span::Span;

use thiserror;
use thiserror::Error;

#[derive(Debug, Clone, Copy)]
pub enum ExpectedArgsCount {
    Count(usize),
    Range { lo: usize, hi: usize },
}

impl std::fmt::Display for ExpectedArgsCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedArgsCount::Count(c) => write!(f, "{c}"),
            ExpectedArgsCount::Range { lo, hi } => write!(f, "{lo}-{hi}"),
        }
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The value '{value}' cannot be used here.")]
pub struct TheValueCannotBeUsedHere {
    #[label(primary)]
    pub span: Span,
    pub value: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{ty1}' is not assignable to type '{ty2}'.")]
pub struct TypeIsNotAssignableToType {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Property '{prop}' in type '{ty1}' is not assignable to the same property in base type '{ty2}'."
)]
pub struct PropertyAInTypeXIsNotAssignableToTheSamePropertyInBaseTypeY {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Argument of type '{arg_ty}' is not assignable to parameter of type '{param_ty}'.")]
pub struct ArgumentOfTyIsNotAssignableToParameterOfTy {
    #[label(primary)]
    pub span: Span,
    pub arg_ty: String,
    pub param_ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected {x} {}arguments, but got {y}.", {
    if *is_ty {
        "type "
    } else {
        ""
    }
})]
pub struct ExpectedXArgsButGotY {
    #[label(primary)]
    pub span: Span,
    pub x: ExpectedArgsCount,
    pub y: usize,
    pub is_ty: bool,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected at least {x} arguments, but got {y}.")]
pub struct ExpectedAtLeastXArgsButGotY {
    #[label(primary)]
    pub span: Span,
    pub x: usize,
    pub y: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot assign to '{name}' because it is a {ty}.")]
pub struct CannotAssignToNameBecauseItIsATy {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The '{op1}' operator is not allowed for boolean types. Consider using '{op2}' instead.")]
pub struct TheOp1IsNotAllowedForBooleanTypesConsiderUsingOp2Instead {
    #[label(primary)]
    pub span: Span,
    pub op1: String,
    pub op2: String,
}

#[derive(Clone, Copy, Debug)]
pub enum LeftOrRight {
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
pub struct TheSideOfAnArithmeticOperationMustBeOfTypeAnyNumberBigintOrAnEnumType {
    #[label(primary)]
    pub span: Span,
    pub left_or_right: LeftOrRight,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Object literal may only specify known properties, and '{prop}' does not exist in type '{ty}'."
)]
pub struct ObjectLitMayOnlySpecifyKnownPropAndFieldDoesNotExist {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Property '{field}' is missing.")]
pub struct PropertyXIsMissing {
    #[label(primary)]
    pub span: Span,
    pub field: String,
    // #[related]
    // pub related: [DefinedHere; 1],
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Type '{ty1}' is missing the following properties from type '{ty2}': {}, and {len} more.", {
        props.join(", ")
    }
)]
pub struct TypeIsMissingTheFollowingPropertiesFromType1Colon2And3More {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
    pub props: Vec<String>,
    pub len: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot create an instance of an abstract class.")]
pub struct CannotCreateAnInstanceOfAnAbstractClass {
    #[label(primary)]
    pub span: Span,
    #[related]
    pub abstract_class_list: Vec<ClassNameHasAbstractModifier>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{} has `abstract` modifier.", {
    if let Some(name) = name {
        format!("Class '{name}'")
    } else {
        "Class".to_string()
    }
})]
#[diagnostic(severity(Warning))]
pub struct ClassNameHasAbstractModifier {
    #[label(primary)]
    pub span: Span,
    pub name: Option<String>,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum DeclKind {
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
pub struct CannotUsedBeforeItsDeclaration {
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
pub struct DefinedHere {
    #[label(primary)]
    pub span: Span,
    pub kind: DeclKind,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Constructor implementation is missing.")]
pub struct ConstructorImplementationIsMissing {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Function implementation is missing or not immediately following the declaration.")]
pub struct FunctionImplementationIsMissingOrNotImmediatelyFollowingTheDeclaration {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Value of type '{ty}' is not callable.")]
#[diagnostic(help("Did you mean to include 'new'?"))]
pub struct ValueOfTypeIsNotCallable {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Property '{prop}' of type '{ty_b}' is not assignable to '{ty_c}' index type '{index_ty_d}'."
)]
pub struct PropertyAOfTypeBIsNotAssignableToCIndexTypeD {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub ty_b: String,
    pub ty_c: String,
    pub index_ty_d: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{decl}' is referenced directly or indirectly in its own base expression.")]
pub struct DeclIsReferencedDirectlyOrIndirectlyInItsOwnBaseExpression {
    #[label(primary)]
    pub span: Span,
    pub decl: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{ty}' index signatures are incompatible.")]
pub struct IndexSignaturesAreIncompatible {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Property '{prop}' does not exist on type '{ty}'.")]
pub struct PropertyXDoesNotExistOnTypeY {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub ty: String,
    #[related]
    pub related: Vec<PropertyXDoesNotExistOnTypeYHelperKind>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
pub enum PropertyXDoesNotExistOnTypeYHelperKind {
    #[error(transparent)]
    #[diagnostic(transparent)]
    DidYourMeanToAccessTheStaticMemberInstead(DidYourMeanToAccessTheStaticMemberInstead),
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Did you mean to access the static member '{class_name}.{prop_name}' instead?")]
#[diagnostic(severity(Advice))]
pub struct DidYourMeanToAccessTheStaticMemberInstead {
    #[label(primary)]
    pub span: Span,
    pub class_name: String,
    pub prop_name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Generic type '{ty}' requires {n} type argument{}.", { if *n > 1 { "s" } else { "" } })]
pub struct GenericTypeXRequiresNTypeArguments {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub n: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Generic type '{ty}' requires between {x} and {y} type arguments.")]
pub struct GenericTypeXRequiresBetweenXAndYTypeArguments {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub x: usize,
    pub y: usize,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{x}' recursively references itself as a base type.")]
pub struct TypeXRecursivelyReferencesItselfAsABaseType {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    #[label = "Base type is defined here."]
    pub base_defined_span: Option<Span>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{ty}' cannot be used to index type '{index_ty}'.")]
pub struct TypeXCannotBeUsedToIndexTypeY {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub index_ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type '{ty}' cannot be used as an index type.")]
pub struct TypeCannotBeUsedAsAnIndexType {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "An index signature parameter type must be 'string', 'number', 'symbol', or a template literal type."
)]
pub struct AnIndexSignatureParameterTypeMustBeStringNumberSymbolOrATemplateLiteralType {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "An index signature parameter type cannot be a literal type or generic type. Consider using a mapped object type instead."
)]
pub struct AnIndexSignatureParameterTypeCannotBeALiteralTypeOrGenericTypeConsiderUsingAMappedObjectTypeInstead
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "A class can only implement an object type or intersection of object types with statically known members."
)]
pub struct AClassCanOnlyImplementAnObjectTypeOrIntersectionOfObjectTypesWithStaticallyKnownMembers {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A class cannot implement a primitive type like '{ty}'.")]
#[diagnostic(help = "It can only implement other named object types.")]
pub struct AClassCannotImplementAPrimTy {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("No overload matches this call.")]
pub struct NoOverloadMatchesThisCall {
    #[label(primary)]
    pub span: Span,
    #[label(collection, "Unmatched call.")]
    pub unmatched_calls: Vec<Span>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("This overload signature is not compatible with its implementation signature.")]
pub struct ThisOverloadSignatureIsNotCompatibleWithItsImplementationSignature {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("This expression is not {}.", if self.is_call {
    "callable"
} else {
    "constructable"
})]
pub struct ThisExpressionIsNotConstructable {
    #[label(primary)]
    span: Span,
    is_call: bool,
}

impl ThisExpressionIsNotConstructable {
    pub fn new_from_call(span: Span) -> Self {
        Self {
            span,
            is_call: true,
        }
    }

    pub fn new_from_constructor(span: Span) -> Self {
        Self {
            span,
            is_call: false,
        }
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type parameter '{ty}' has a circular constraint.")]
pub struct TypeParameterXHasACircularConstraint {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot assign to '{prop}' because it is a read-only property.")]
pub struct CannotAssignToXBecauseItIsAReadOnlyProperty {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The left-hand side of a 'for...in' statement must be of type 'string' or 'any'.")]
pub struct TheLeftHandSideOfAForInStatementMustBeOfTypeStringOrAny {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter."
)]
pub struct TheLeftHandSideOfAnInstanceofExpressionMustBeOfTypeAnyAnObjectTypeOrATypeParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "The right-hand side of a 'for...in' statement must be of type 'any', an object type or a type parameter, but here has type '{ty}'."
)]
pub struct TheRightHandSideOfAForInStatementMustBeOfTypeAnyAnObjectTypeOrATypeParameterButHereHasType
{
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type produces a tuple type that is too large to represent.")]
pub struct TypeProducesATupleTypeThatIsTooLargeToRepresent {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Interface '{derived}' incorrectly extends interface '{base}'.")]
pub struct InterfaceDerivedIncorrectlyExtendsInterfaceBase {
    #[label(primary)]
    pub span: Span,
    pub base: String,
    pub derived: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Ambient module declaration cannot specify relative module name.")]
pub struct AmbientModuleDeclarationCannotSpecifyRelativeModuleName {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A rest parameter must be of an array type.")]
pub struct ARestParameterMustBeOfAnArrayType {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'this' cannot be referenced in a module or namespace body.")]
pub struct ThisCannotBeReferencedInAModuleOrNamespaceBody {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Module '\"{module_name}\"' declares '{symbol_name}' locally, but it is not exported.")]
pub struct ModuleADeclaresBLocallyButItIsNotExported {
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
pub struct NameIsDeclaredHere {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error(
    "Module '\"{module_name}\"' declares '{symbol_name}' locally, but it is exported as '{target_name}'."
)]
pub struct ModuleADeclaresBLocallyButItIsExportedAsC {
    #[label(primary)]
    pub span: Span,
    pub module_name: String,
    pub symbol_name: String,
    pub target_name: String,
    #[related]
    pub related: Vec<ModuleADeclaresBLocallyButItIsExportedAsCHelperKind>,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
pub enum ModuleADeclaresBLocallyButItIsExportedAsCHelperKind {
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
pub struct ExportedAliasHere {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Module '{module}' has no exported member '{member}'.")]
pub struct ModuleXHasNoExportedMemberY {
    #[label(primary)]
    pub span: Span,
    pub module: String,
    pub member: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Namespace '{namespace}' has no exported member '{member}'.")]
pub struct NamespaceXHasNoExportedMemberY {
    #[label(primary)]
    pub span: Span,
    pub namespace: String,
    pub member: String,
}

#[derive(Debug, Default)]
pub enum UndefinedOrNull {
    Undefined,
    Null,
    #[default]
    Both,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{name}' is possibly {}.", {
    match kind {
        UndefinedOrNull::Both => "'undefined' or 'null'",
        UndefinedOrNull::Undefined => "'undefined'",
        UndefinedOrNull::Null => "'null'",
    }
})]
pub struct XIsPossiblyNullOrUndefined {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    pub kind: UndefinedOrNull,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' is not a constructor function type.")]
pub struct TypeXIsNotAConstructorFunctionType {
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
pub struct TheOperandOfAnIncrementOrDecrementOperatorMustBeAVariableOrAPropertyAccess {
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
pub struct TheOperandOfAnIncrementOrDecrementOperatorMayNotBeAnOptionalPropertyAccess {
    #[label(primary)]
    pub span: Span,
    pub is_incr: bool,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Declaration name conflicts with built-in global identifier '{name}'.")]
pub struct DeclarationNameConflictsWithBuiltInGlobalIdentifier {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Setters cannot return a value.")]
pub struct SettersCannotReturnAValue {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Augmentations for the global scope can only be directly nested in external modules or ambient module declarations."
)]
pub struct AugmentationsForTheGlobalScopeCanOnlyBeDirectlyNestedInExternalModulesOrAmbientModuleDeclarations
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Property '{prop}' is private and only accessible within class '{class}'.")]
pub struct PropertyIsPrivateAndOnlyAccessibleWithinClass {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub class: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Interface '{interface}' cannot simultaneously extend types '{ty1}' and '{ty2}'.")]
pub struct InterfaceCannotSimultaneouslyExtendTypes1And2 {
    #[label(primary)]
    pub span: Span,
    pub interface: String,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Duplicate identifier '{ident}'.")]
pub struct DuplicateIdentifier {
    #[label(primary)]
    pub span: Span,
    pub ident: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Duplicate function implementation.")]
pub struct DuplicateFunctionImplementation {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{x}' has no matching index signature for type '{y}'.")]
pub struct TypeXHasNoMatchingIndexSignatureForTypeY {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Cannot extend a class '{class}'. Class constructor is marked as private.")]
pub struct CannotExtendAClass0ClassConstructorIsMarkedAsPrivate {
    #[label(primary)]
    pub span: Span,
    pub class: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Enum member must have initializer.")]
pub struct EnumMemberMustHaveInitializer {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Conversion of type '{source_ty}' to type '{target_ty}' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first."
)]
pub struct ConversionOfType0ToType1MayBeAMistakeBecauseNeitherTypeSufficientlyOverlapsWithTheOtherIfThisWasIntentionalConvertTheExpressionToUnknownFirst
{
    #[label(primary)]
    pub span: Span,
    pub source_ty: String,
    pub target_ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Types of parameters '{ty_x}' and '{ty_y}' are incompatible.")]
pub struct TypesOfParametersXAndYAreIncompatible {
    #[label(primary)]
    pub span: Span,
    pub ty_x: String,
    pub ty_y: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' provides no match for the signature '{sig}'.")]
pub struct TypeXProvidesNoMatchForTheSignatureY {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub sig: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Operator '{op}' cannot be applied to types '{ty1}' and '{ty2}'.")]
pub struct OperatorCannotBeAppliedToTypesXAndY {
    #[label(primary)]
    pub span: Span,
    pub op: String,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("A 'get' accessor must return a value.")]
pub struct AGetAccessorMustReturnAValue {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type instantiation is excessively deep and possibly infinite.")]
pub struct TypeInstantiationIsExcessivelyDeepAndPossiblyInfinite {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'super' can only be referenced in a derived class.")]
pub struct SuperCanOnlyBeReferencedInADerivedClass {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'super' cannot be referenced in a computed property name.")]
pub struct SuperCannotBeReferencedInAComputedPropertyName {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Super calls are not permitted outside constructors or in nested functions inside constructors."
)]
pub struct SuperCallsAreNotPermittedOutsideConstructorsOrInNestedFunctionsInsideConstructors {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'super' can only be referenced in members of derived classes or object literal expressions."
)]
pub struct SuperCanOnlyBeReferencedInMembersOfDerivedClassesOrObjectLiteralExpressions {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'super' property access is permitted only in a constructor, member function, or member accessor of a derived class."
)]
pub struct SuperPropertyAccessIsPermittedOnlyInAConstructorMemberFunctionOrMemberAccessorOfADerivedClass
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element."
)]
pub struct InAnEnumWithMultipleDeclarationsOnlyOneDeclarationCanOmitAnInitializerForItsFirstEnumElement
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Non-abstract class expression does not implement inherited abstract member '{member}' from class '{class}'."
)]
pub struct NonAbstractClassExpressionDoesNotImplementInheritedAbstractMember0FromClass1 {
    #[label(primary)]
    pub span: Span,
    pub member: String,
    pub class: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Non-abstract class '{non_abstract_class}' does not implement inherited abstract member '{member}' from class '{abstract_class}'."
)]
pub struct NonAbstractClass0DoesNotImplementInheritedAbstractMember1FromClass2 {
    #[label(primary)]
    pub span: Span,
    pub non_abstract_class: String,
    pub member: String,
    pub abstract_class: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("An arithmetic operand must be of type 'any', 'number', 'bigint' or an enum type.")]
pub struct AnArithmeticOperandMustBeOfTypeAnyNumberBigintOrAnEnumType {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("A computed property name must be of type 'string', 'number', 'symbol' or 'any'.")]
pub struct AComputedPropertyNameMustBeOfTypeStringNumberSymbolOrAny {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("The operand of a 'delete' operator cannot be a read-only property.")]
pub struct TheOperandOfADeleteOperatorCannotBeAReadOnlyProperty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Property '{name}' is used before its initialization.")]
pub struct Property0IsUsedBeforeItsInitialization {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Constructors for derived classes must contain a 'super' call.")]
pub struct ConstructorsForDerivedClassesMustContainASuperCall {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("A constructor cannot contain a 'super' call when its class extends 'null'.")]
pub struct AConstructorCannotContainASuperCallWhenItsClassExtendsNull {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'super' must be called before accessing 'this' in the constructor of a derived class.")]
pub struct SuperMustBeCalledBeforeAccessingThisInTheConstructorOfADerivedClass {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Only public and protected methods of the base class are accessible via the 'super' keyword."
)]
pub struct OnlyPublicAndProtectedMethodsOfTheBaseClassAreAccessibleViaTheSuperKeyword {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "A namespace declaration cannot be located prior to a class or function with which it is merged."
)]
pub struct ANamespaceDeclarationCannotBeLocatedPriorToAClassOrFunctionWithWhichItIsMerged {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Cannot access '{name}.{prop_name}' because '{name}' is a type, but not a namespace. Did you mean to retrieve the type of the property '{prop_name}' in '{name}' with '{name}[\"{prop_name}\"]'?"
)]
pub struct CannotAccessPropNameBecauseXIsATypeButNotANamespaceDidYouMeanToRetrieveTheTypeOfThePropertyInX
{
    #[label(primary)]
    pub span: Span,
    pub name: String,
    pub prop_name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "It is likely that you are missing a comma to separate these two template expressions. They form a tagged template expression which cannot be invoked."
)]
pub struct ItIsLikelyThatYouAreMissingACommaToSeparateTheseTwoTemplateExpressionsTheyFormATaggedTemplateExpressionWhichCannotBeInvoked
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Overload signatures must all be ambient or non-ambient.")]
pub struct OverloadSignaturesMustAllBeAmbientOrNonAmbient {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value."
)]
pub struct AFunctionWhoseDeclaredTypeIsNeitherUndefinedVoidNorAnyMustReturnAValue {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Function lacks ending return statement and return type does not include 'undefined'.")]
pub struct FunctionLacksEndingReturnStatementAndReturnTypeDoesNotIncludeUndefined {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("The operand of a 'delete' operator must be a property reference.")]
pub struct TheOperandOfADeleteOperatorMustBeAPropertyReference {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Untyped function calls may not accept type arguments.")]
pub struct UntypedFunctionCallsMayNotAcceptTypeArguments {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Class static side '{class}' incorrectly extends base class static side '{base}'.")]
pub struct ClassStaticSideXIncorrectlyExtendsBaseClassStaticSideY {
    #[label(primary)]
    pub span: Span,
    pub class: String,
    pub base: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Subsequent variable declarations must have the same type. Variable '{var}' must be of type '{ty1}', but here has type '{ty2}'."
)]
pub struct SubsequentVariableDeclarationsMustHaveTheSameTypeVariableMustBeOfTypeXButHereHasTypeY {
    #[label(primary)]
    pub span: Span,
    pub var: String,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty1}' is not comparable to type '{ty2}'.")]
pub struct TypeXIsNotComparableToTypeY {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' is not generic.")]
pub struct TypeXIsNotGeneric {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Cannot export '{spec}'. Only local declarations can be exported from a module.")]
pub struct CannotExportXOnlyLocalDeclarationsCanBeExportedFromAModule {
    #[label(primary)]
    pub span: Span,
    pub spec: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Multiple constructor implementations are not allowed.")]
pub struct MultipleConstructorImplementationsAreNotAllowed {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'{item}' refers to a value, but is being used as a type here. Did you mean 'typeof {item}'?"
)]
pub struct XRefersToAValueButIsBeingUsedAsATypeHereDidYouMeanTypeofX {
    #[label(primary)]
    pub span: Span,
    pub item: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'this' implicitly has type 'any' because it does not have a type annotation.")]
pub struct ThisImplicitlyHasTypeAnyBecauseItDoesNotHaveATypeAnnotation {
    #[label(primary)]
    pub span: Span,
    #[related]
    pub related: Option<AnOuterValueOfThisIsShadowedByThisContainer>,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("An outer value of 'this' is shadowed by this container.")]
#[diagnostic(severity(Warning))]
pub struct AnOuterValueOfThisIsShadowedByThisContainer {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Cannot redeclare exported variable '{name}'.")]
pub struct CannotRedeclareExportedVariableX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Mapped object type implicitly has an 'any' template type.")]
pub struct MappedObjectTypeImplicitlyHasAnAnyTemplateType {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("All declarations of '{symbol}' must have identical modifiers.")]
pub struct AllDeclarationsOfXMustHaveIdenticalModifiers {
    #[label(primary)]
    pub span: Span,
    pub symbol: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Tuple type '{x}' of length '{y}' has no element at index '{z}'.")]
pub struct TupleTypeXOfLengthYHasNoElementAtIndexZ {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: usize,
    pub z: usize,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Object is of type 'unknown'.")]
pub struct ObjectIsOfTypeUnknown {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' is not an array type.")]
pub struct TypeXIsNotAnArrayType {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Cannot invoke an object which is possibly 'null' or 'undefined'.")]
pub struct CannotInvokeAnObjectWhichIsPossiblyNullOrUndefined {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Cannot invoke an object which is possibly 'undefined'.")]
pub struct CannotInvokeAnObjectWhichIsPossiblyUndefined {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Cannot invoke an object which is possibly 'null'.")]
pub struct CannotInvokeAnObjectWhichIsPossiblyNull {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Not all code paths return a value.")]
pub struct NotAllCodePathsReturnAValue {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Rest types may only be created from object types.")]
pub struct RestTypesMayOnlyBeCreatedFromObjectTypes {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "A computed property name in a class property declaration must have a simple literal type or a 'unique symbol' type."
)]
pub struct AComputedPropertyNameInAClassPropertyDeclarationMustHaveASimpleLiteralTypeOrAUniqueSymbolType
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "A computed property name in an interface must refer to an expression whose type is a literal type or a 'unique symbol' type."
)]
pub struct AComputedPropertyNameInAnInterfaceMustReferToAnExpressionWhoseTypeIsALiteralTypeOrAUniqueSymbolType
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Object literal's property '{prop}' implicitly has an '{ty}' type.")]
pub struct ObjectLiteralSPropertyXImplicitlyHasAnYType {
    #[label(primary)]
    pub span: Span,
    pub prop: String,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{x}', which lacks return-type annotation, implicitly has an '{y}' return type.")]
pub struct XWhichLacksReturnTypeAnnotationImplicitlyHasAnYReturnType {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Parameter '{parameter}' implicitly has an '{ty}' type.")]
pub struct ParameterImplicitlyHasAn1Type {
    #[label(primary)]
    pub span: Span,
    pub parameter: String,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Rest parameter '{parameter}' implicitly has an 'any[]' type.")]
pub struct RestParameterXImplicitlyHasAnAnyType {
    #[label(primary)]
    pub span: Span,
    pub parameter: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "An interface can only extend an object type or intersection of object types with statically known members."
)]
pub struct AnInterfaceCanOnlyExtendAnObjectTypeOrIntersectionOfObjectTypesWithStaticallyKnownMembers
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Class '{class}' incorrectly implements interface '{interface}'.")]
pub struct ClassXIncorrectlyImplementsInterfaceY {
    #[label(primary)]
    pub span: Span,
    pub class: String,
    pub interface: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Class declaration cannot implement overload list for '{name}'.")]
pub struct ClassDeclarationCannotImplementOverloadListForX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Function with bodies can only merge with classes that are ambient.")]
pub struct FunctionWithBodiesCanOnlyMergeWithClassesThatAreAmbient {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("All declarations of '{name}' must have identical type parameters.")]
pub struct AllDeclarationsOfXMustHaveIdenticalTypeParameters {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Property '{property}' has no initializer and is not definitely assigned in the constructor."
)]
pub struct PropertyXHasNoInitializerAndIsNotDefinitelyAssignedInTheConstructor {
    #[label(primary)]
    pub span: Span,
    pub property: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An object literal cannot have multiple properties with the same name.")]
pub struct AnObjectLiteralCannotHaveMultiplePropertiesWithTheSameName {
    #[label(primary)]
    pub span: Span,
    #[label("Previous definition here")]
    pub old: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Variable '{name}' is used before being assigned.")]
pub struct VariableXIsUsedBeforeBeingAssigned {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Property '{name}' is used before being assigned.")]
pub struct PropertyXIsUsedBeforeBeingAssigned {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Class '{x}' incorrectly extends base class '{y}'.")]
pub struct ClassXIncorrectlyExtendsBaseClassY {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{x}' does not satisfy the constraint '{y}'.")]
pub struct TypeXDoesNotSatisfyTheConstraintY {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Right operand of ?? is unreachable because the left operand is never nullish.")]
pub struct RightOperandOfIsUnreachableBecauseTheLeftOperandIsNeverNullish {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("This expression is always nullish.")]
pub struct ThisExpressionIsAlwaysNullish {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'yield' expression implicitly results in an 'any' type because its containing generator lacks a return-type annotation."
)]
pub struct YieldExpressionImplicitlyResultsInAnAnyTypeBecauseItsContainingGeneratorLacksAReturnTypeAnnotation
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Element implicitly has an 'any' type because index expression is not of type 'number'.")]
pub struct ElementImplicitlyHasAnAnyTypeBecauseIndexExpressionIsNotOfTypeNumber {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Element implicitly has an 'any' type because expression of type '{x}' can't be used to index type '{y}'."
)]
pub struct ElementImplicitlyHasAnAnyTypeBecauseExpressionOfTypeXCanTBeUsedToIndexTypeY {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'super' must be called before accessing a property of 'super' in the constructor of a derived class."
)]
pub struct SuperMustBeCalledBeforeAccessingAPropertyOfSuperInTheConstructorOfADerivedClass {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'super' cannot be referenced in constructor arguments.")]
pub struct SuperCannotBeReferencedInConstructorArguments {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("A 'this' type is available only in a non-static member of a class or interface.")]
pub struct AThisTypeIsAvailableOnlyInANonStaticMemberOfAClassOrInterface {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' must have a '[Symbol.iterator]()' method that returns an iterator.")]
pub struct TypeMustHaveASymbolIteratorMethodThatReturnsAnIterator {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("An expression of type 'void' cannot be tested for truthiness.")]
pub struct AnExpressionOfTypeVoidCannotBeTestedForTruthiness {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("This kind of expression is always truthy.")]
pub struct ThisKindOfExpressionIsAlwaysTruthy {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("This kind of expression is always falsy.")]
pub struct ThisKindOfExpressionIsAlwaysFalsy {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{x}' implicitly has an '{y}' return type, but a better type may be inferred from usage.")]
pub struct XImplicitlyHasAnYReturnTypeButABetterTypeMayBeInferredFromUsage {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{field}' defined by the parent class is not accessible in the child class via super.")]
pub struct AbstractMethod0InClass1CannotBeAccessedViaSuperExpression {
    #[label(primary)]
    pub span: Span,
    pub field: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Import declaration conflicts with local declaration of '{name}'.")]
pub struct ImportDeclarationConflictsWithLocalDeclarationOfX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Export declaration conflicts with exported declaration of '{name}'.")]
pub struct ExportDeclarationConflictsWithExportedDeclarationOfX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Export declarations are not permitted in a namespace.")]
pub struct ExportDeclarationsAreNotPermittedInANamespace {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("In ambient enum declarations member initializer must be constant expression.")]
pub struct InAmbientEnumDeclarationsMemberInitializerMustBeConstantExpression {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("An export assignment cannot be used in a module with other exported elements.")]
pub struct AnExportAssignmentCannotBeUsedInAModuleWithOtherExportedElements {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Get and set accessors in a class must both be abstract or non-abstract.")]
pub struct AccessorsMustBothBeAbstractOrNonAbstract {
    #[label(primary)]
    pub getter_span: Span,
    #[label = "Setter declaration is defined here."]
    pub setter_span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Get accessor must be at least as accessible as the set accessor.")]
pub struct AGetAccessorMustBeAtLeastAsAccessibleAsTheSetter {
    #[label(primary)]
    pub getter_span: Span,
    #[label = "Setter declaration is defined here."]
    pub setter_span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type arguments for '{name}' circularly reference themselves.")]
pub struct TypeArgumentsForXCircularlyReferenceThemselves {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Tuple type arguments circularly reference themselves.")]
pub struct TupleTypeArgumentsCircularlyReferenceThemselves {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Circular definition of import alias '{name}'.")]
pub struct CircularDefinitionOfImportAliasX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{name}' is referenced directly or indirectly in its own type annotation.")]
pub struct XIsReferencedDirectlyOrIndirectlyInItsOwnTypeAnnotation {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'{name}' implicitly has type 'any' because it does not have a type annotation and is referenced directly or indirectly in its own initializer."
)]
pub struct XImplicitlyHasTypeAnyBecauseItDoesNotHaveATypeAnnotationAndIsReferencedDirectlyOrIndirectlyInItsOwnInitializer
{
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Class '{class_name}' defines instance member accessor '{property_name}', but extended class '{extended_class_name}' defines it as instance member function."
)]
pub struct ClassDefinesInstanceMemberAccessorButExtendedClassDefinesItAsInstanceMemberFunction {
    #[label(primary)]
    pub span: Span,
    pub class_name: String,
    pub property_name: String,
    pub extended_class_name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Class '{class_name}' defines instance member property '{property_name}', but extended class '{extended_class_name}' defines it as instance member function."
)]
pub struct ClassDefinesInstanceMemberProperButExtendedClassDefinesItAsInstanceMemberFunction {
    #[label(primary)]
    pub span: Span,
    pub class_name: String,
    pub property_name: String,
    pub extended_class_name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Value of type '{ty1}' has no properties in common with type '{ty2}'. Did you mean to call it?"
)]
pub struct ValueOfTypeHasNoPropertiesInCommonWithTypeDidYouMeanToCallIt {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty1}' has no properties in common with type '{ty2}'.")]
pub struct TypeXHasNoPropertiesInCommonWithTypeY {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("This condition will always return '{result}'.")]
pub struct ThisConditionWillAlwaysReturnX {
    #[label(primary)]
    pub span: Span,
    pub result: bool,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Cannot assign to '{name}' because it is a constant.")]
pub struct CannotAssignToXBecauseItIsAConstant {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Index signature in type '{ty}' only permits reading.")]
pub struct IndexSignatureInTypeXOnlyPermitsReading {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Merged declaration '{decl}' cannot include a default export declaration. Consider adding a separate 'export default {decl}' declaration instead."
)]
pub struct MergedDeclarationCannotIncludeADefaultExportDeclarationConsiderAddingASeparateExportDefaultDeclarationInstead
{
    #[label(primary)]
    pub span: Span,
    pub decl: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Individual declarations in merged declaration '{decl}' must be all exported or all local."
)]
pub struct IndividualDeclarationsInMergedDeclarationMustBeAllExportedOrAllLocal {
    #[label(primary)]
    pub span: Span,
    pub decl: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "The intersection '{ty}' was reduced to 'never' because property '{prop}' has conflicting types in some constituents."
)]
pub struct TheIntersectionTyWasReducedToNeverBecausePropertyHasConflictingTypesInSomeConstituents {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
    pub prop: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' is generic and can only be indexed for reading.")]
pub struct TypeIsGenericAndCanOnlyBeIndexedForReading {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Constructor of class '{class}' is {visible} and only accessible within the class declaration."
)]
pub struct ConstructorOfClassXIsPrivateOrProtectedAndOnlyAccessibleWithinTheClassDeclaration {
    #[label(primary)]
    pub span: Span,
    pub class: String,
    pub visible: &'static str,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Type '{ty}' must have a '[Symbol.iterator]()' method that returns an iterator.")]
pub struct TypeXMustHaveASymbolIteratorMethodThatReturnsAnIterator {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'new' expression, whose target lacks a construct signature, implicitly has an 'any' type."
)]
pub struct NewExpressionWhoseTargetLacksAConstructSignatureImplicitlyHasAnAnyType {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "A 'super' call must be the first statement in the constructor to refer to 'super' or 'this' when a derived class contains initialized properties, parameter properties, or private identifiers."
)]
pub struct ASuperCallMustBeTheFirstStatementInTheConstructorToReferToSuperOrThisWhenADerivedClassContainsInitializedPropertiesParameterPropertiesOrPrivateIdentifiers
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "This condition will always return '{return_value}' since JavaScript compares objects by reference, not value."
)]
pub struct ThisConditionWillAlwaysReturnXSinceJavaScriptComparesObjectsByReferenceNotValue {
    #[label(primary)]
    pub span: Span,
    pub return_value: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "A 'const' assertion can only be applied to references to enum members, or string, number, boolean, array, or object literals."
)]
pub struct AConstAssertionCanOnlyBeAppliedToReferencesToEnumMembersOrStringNumberBooleanArrayOrObjectLiterals
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "A member initializer in a enum declaration cannot reference members declared after it, including members defined in other enums."
)]
pub struct AMemberInitializerInAEnumDeclarationCannotReferenceMembersDeclaredAfterItIncludingMembersDefinedInOtherEnums
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "No overload expects {argument_count} arguments, but overloads do exist that expect either {max_below} or {min_above} arguments."
)]
pub struct NoOverloadExpectsXArgumentsButOverloadsDoExistThatExpectEitherAOrBArguments {
    #[label(primary)]
    pub span: Span,
    pub argument_count: usize,
    pub max_below: usize,
    pub min_above: usize,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("A spread argument must either have a tuple type or be passed to a rest parameter.")]
pub struct ASpreadArgumentMustEitherHaveATupleTypeOrBePassedToARestParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "No overload expects {type_argument_count} type arguments, but overloads do exist that expect either {max_below} or {min_above} type arguments."
)]
pub struct NoOverloadExpectsXTypeArgumentsButOverloadsDoExistThatExpectEitherAOrBTypeArguments {
    #[label(primary)]
    pub span: Span,
    pub type_argument_count: usize,
    pub max_below: usize,
    pub min_above: usize,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("'{ty1}' index type '{ty2}' is not assignable to '{ty3}' index type '{ty4}'.")]
pub struct AIndexTypeBIsNotAssignableToCIndexTypeD {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
    pub ty3: String,
    pub ty4: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "The right-hand side of an 'instanceof' expression must not be an instantiation expression."
)]
pub struct TheRightHandSideOfAnInstanceofExpressionMustNotBeAnInstantiationExpression {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Function overload must be static.")]
pub struct FunctionOverloadMustBeStatic {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "This comparison appears to be unintentional because the types '{ty1}' and '{ty2}' have no overlap."
)]
pub struct ThisComparisonAppearsToBeUnintentionalBecauseTheTypesXAndYHaveNoOverlap {
    #[label(primary)]
    pub span: Span,
    pub ty1: String,
    pub ty2: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'{x}' is defined as an accessor in class '{class_y}', but is overridden here in '{class_z}' as an instance property."
)]
pub struct XIsDefinedAsAnAccessorInClassYButIsOverriddenHereInZAsAnInstanceProperty {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub class_y: String,
    pub class_z: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'{x}' is defined as a property in class '{class_y}', but is overridden here in '{class_z}' as an accessor."
)]
pub struct XIsDefinedAsAPropertyInClassYButIsOverriddenHereInZAsAnAccessor {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub class_y: String,
    pub class_z: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Duplicate index signature for type '{ty}'.")]
pub struct DuplicateIndexSignatureForTypeX {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Type '{ty}' may represent a primitive value, which is not permitted as the right operand of the 'in' operator."
)]
pub struct TypeXMayRepresentAPrimitiveValueWhichIsNotPermittedAsTheRightOperandOfTheInOperator {
    #[label(primary)]
    pub span: Span,
    pub ty: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Return type annotation circularly references itself.")]
pub struct ReturnTypeAnnotationCircularlyReferencesItself {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "'{name}' implicitly has return type 'any' because it does not have a return type annotation and is referenced directly or indirectly in one of its return expressions."
)]
pub struct XImplicitlyHasReturnTypeAnyBecauseItDoesNotHaveAReturnTypeAnnotationAndIsReferencedDirectlyOrIndirectlyInOneOfItsReturnExpressions
{
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Function implicitly has return type 'any' because it does not have a return type annotation and is referenced directly or indirectly in one of its return expressions."
)]
pub struct FunctionImplicitlyHasReturnTypeAnyBecauseItDoesNotHaveAReturnTypeAnnotationAndIsReferencedDirectlyOrIndirectlyInOneOfItsReturnExpressions
{
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error(
    "Property '{property}' is protected and only accessible within class '{class}' and its subclasses."
)]
pub struct PropertyXIsProtectedAndOnlyAccessibleWithinClassYAndItsSubclasses {
    #[label(primary)]
    pub span: Span,
    pub property: String,
    pub class: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt, Default)]
#[error("Overload signatures must all be public, private or protected.")]
pub struct OverloadSignaturesMustAllBePublicPrivateOrProtected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Cannot find name '{name}'.")]
pub struct CannotFindName {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "This condition will always return true since this function is always defined. Did you mean to call it instead?"
)]
pub struct ThisConditionWillAlwaysReturnTrueSinceThisFunctionIsAlwaysDefinedDidYouMeanToCallItInstead
{
    #[label(primary)]
    pub span: Span,
}
