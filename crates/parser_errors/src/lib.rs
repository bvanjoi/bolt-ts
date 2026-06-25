use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_span::Span;

use thiserror;
use thiserror::Error;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Syntax Error: Unexpected token ','")]
pub struct ClassesCanOnlyExtendASingleClass {
    #[label(primary)]
    pub span: Span,
    #[label("Classes can only extend a single class.")]
    pub extra_extends: Option<Span>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type parameter list cannot be empty.")]
pub struct TypeParameterListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Octal literals are not allowed.")]
#[diagnostic(help = "Use the syntax '{help_lit}'.")]
pub struct OctalLiteralsAreNotAllowed {
    #[label(primary)]
    pub span: Span,
    pub help_lit: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Decimals with leading zeros are not allowed.")]
pub struct DecimalsWithLeadingZerosAreNotAllowed {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Numeric separators are not allowed here.")]
pub struct NumericSeparatorsAreNotAllowedHere {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Variable declaration list cannot be empty.")]
pub struct VariableDeclarationListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{kind}")]
pub struct MissingIdent {
    #[label(primary)]
    pub span: Span,
    pub kind: MissingIdentKind,
}

#[derive(Error, Diagnostic, Debug)]
pub enum MissingIdentKind {
    #[error("Identifier expected.")]
    IdentifierExpected,
    #[error("Expression expected.")]
    ExpressionExpected,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type argument list cannot be empty.")]
pub struct TypeArgumentListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A parameter property may not be declared using a binding pattern.")]
pub struct AParameterPropertyMayNotBeDeclaredUsingABindingPattern {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{} cannot be declared using a rest parameter.", {kinds.iter().map(|k| format!("'{k}'")).collect::<Vec<_>>().join(", ")})]
pub struct AParameterPropertyCannotBeDeclaredUsingARestParameter {
    #[label(primary)]
    pub span: Span,
    pub kinds: Vec<bolt_ts_ast::ModifierKind>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected '{x}'.")]
pub struct ExpectX {
    #[label(primary)]
    pub span: Span,
    pub x: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ClauseKind {
    Extends,
    Implements,
}

impl std::fmt::Display for ClauseKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ClauseKind::Extends => "extends",
            ClauseKind::Implements => "implements",
        };
        write!(f, "{s}")
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{kind}' clause already seen.")]
pub struct ClauseAlreadySeen {
    #[label(primary)]
    pub span: Span,
    pub kind: ClauseKind,
    #[label("It was first defined here.")]
    pub origin: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'extends' clause must precede 'implements' clause.")]
pub struct ExtendsClauseMustPrecedeImplementsClause {
    #[label(primary)]
    pub extends_span: Span,
    #[label("'implements' clause defined here.")]
    pub implements_span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{expected}' expected.")]
pub struct KindExpected {
    #[label(primary)]
    pub span: Span,
    pub expected: String,
    #[related]
    pub related: Option<ExpectedToFindAToMatchTheBTokenHere>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected to find a '{expected}' to match the '{found}' token here.")]
#[diagnostic(severity(Advice))]
pub struct ExpectedToFindAToMatchTheBTokenHere {
    #[label(primary)]
    pub span: Span,
    pub expected: String,
    pub found: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Argument expression expected.")]
pub struct ArgumentExpressionExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature must have a type annotation.")]
pub struct AnIndexSignatureMustHaveATypeAnnotation {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{kind}' modifier cannot appear on a parameter.")]
pub struct ModifierCannotAppearOnAParameter {
    #[label(primary)]
    pub span: Span,
    pub kind: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{kind}' modifier cannot appear on an index signature.")]
pub struct ModifierCannotAppearOnAnIndexSignature {
    #[label(primary)]
    pub span: Span,
    pub kind: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An implementation cannot be declared in type contexts.")]
pub struct AnImplementationCannotBeDeclaredInTypeContexts {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An element access expression should take an argument.")]
pub struct AnElementAccessExpressionShouldTakeAnArgument {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Declaration or statement expected.")]
pub struct DeclarationOrStatementExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An object member cannot be declared optional.")]
pub struct AnObjectMemberCannotBeDeclaredOptional {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A '{}' statement can only be used within an enclosing iteration{} statement.", {
    if self.is_continue {
        "continue"
    } else {
        "break"
    }
}, {
    if self.is_continue {
        ""
    } else {
        " or switch"
    }
})]
pub struct AContinueOrBreakStatementCanOnlyBeUsedWithinAnEnclosingIterationStatement {
    #[label(primary)]
    pub span: Span,
    pub is_continue: bool,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{} property cannot have an initializer.", {
    if self.interface {
        "An interface"
    } else {
        "A type literal"
    }
})]
pub struct AnInterfaceOrTypeLitPropertyCannotHaveAnInitializer {
    #[label(primary)]
    pub span: Span,
    pub interface: bool,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("An enum member cannot have a numeric name.")]
pub struct AnEnumMemberCannotHaveANumericName {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Accessibility modifier already seen.")]
pub struct AccessibilityModifierAlreadySeen {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A '{modifier}' modifier cannot be used with an import declaration.")]
pub struct AModifierCannotBeUsedWithAnImportDeclaration {
    #[label(primary)]
    pub span: Span,
    pub modifier: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("{modifier} modifier already seen.")]
pub struct ModifierAlreadySeen {
    #[label(primary)]
    pub span: Span,
    pub modifier: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Unexpected token. Did you mean `{{'}}'}}` or `&rbrace;`?")]
pub struct UnexpectedTokenDidYouMeanOrRBrace {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Unexpected token. Did you mean `{{'>'}}` or `&gt;`?")]
pub struct UnexpectedTokenDidYouMeanOrGt {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Tagged template expressions are not permitted in an optional chain.")]
pub struct TaggedTemplateExpressionsAreNotPermittedInAnOptionalChain {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Expected corresponding closing tag for JSX fragment.")]
pub struct ExpectedCorrespondingClosingTagForJsxFragment {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Unterminated string literal.")]
pub struct UnterminatedStringLiteral {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Unterminated template literal.")]
pub struct UnterminatedTemplateLiteral {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{{' or JSX element expected.")]
pub struct OrJsxElementExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected corresponding JSX closing tag for '{opening_tag_name}'.")]
pub struct ExpectedCorrespondingJsxClosingTagForOpeningTagName {
    #[label(primary)]
    pub span: Span,
    pub opening_tag_name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Invalid character.")]
pub struct InvalidCharacter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Unicode escape sequence cannot appear here.")]
pub struct UnicodeEscapeSequenceCannotAppearHere {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Digit expected.")]
pub struct DigitExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Keywords cannot contain escape characters.")]
pub struct KeywordsCannotContainEscapeCharacters {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "'arguments' cannot be referenced in {}.", {
        if self.in_property_initializer {
            "property initializers"
        } else {
            "class static initialization blocks"
        }
    }
)]
pub struct ArgumentsCannotBeReferenced {
    #[label(primary)]
    span: Span,
    in_property_initializer: bool,
}

impl ArgumentsCannotBeReferenced {
    pub fn new_in_property_initializer(span: Span) -> Self {
        Self {
            span,
            in_property_initializer: true,
        }
    }

    pub fn new_in_class_static_initialization_block(span: Span) -> Self {
        Self {
            span,
            in_property_initializer: false,
        }
    }
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Property assignment expected.")]
pub struct PropertyAssignmentExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Property or signature expected.")]
pub struct PropertyOrSignatureExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A 'return' statement can only be used within a function body.")]
pub struct AReturnStatementCanOnlyBeUsedWithinAFunctionBody {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Modifier cannot be used here.")]
pub struct ModifierCannotBeUsedHere {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A class member cannot have the '{modifier}' keyword.")]
pub struct AClassMemberCannotHaveTheModifierKeyword {
    #[label(primary)]
    pub span: Span,
    pub modifier: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Identifier expected. '{identifier}' is a reserved word in strict mode.")]
pub struct IdentifierExpectedXIsAReservedWordInStrictMode {
    #[label(primary)]
    pub span: Span,
    pub identifier: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature cannot have a trailing comma.")]
pub struct AnIndexSignatureCannotHaveATrailingComma {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature must have exactly one parameter.")]
pub struct AnIndexSignatureMustHaveExactlyOneParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Declarations must be initialized.")]
pub struct DeclarationsMustBeInitialized {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The left-hand side of a 'for...{}' statement cannot use a type annotation.", {
    if self.is_for_in {
        "in"
    } else {
        "of"
    }
})]
pub struct TheLeftHandSideOfAForInOfStatementCannotUseATypeAnnotation {
    #[label(primary)]
    pub span: Span,
    pub is_for_in: bool,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Line break not permitted here.")]
pub struct LineBreakNotPermittedHere {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature parameter cannot have an accessibility modifier.")]
pub struct AnIndexSignatureParameterCannotHaveAnAccessibilityModifier {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature parameter cannot have a question mark.")]
pub struct AnIndexSignatureParameterCannotHaveAQuestionMark {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature parameter cannot have an initializer.")]
pub struct AnIndexSignatureParameterCannotHaveAnInitializer {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Jump target cannot cross function boundary.")]
pub struct JumpTargetCannotCrossFunctionBoundary {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A default export can only be used in an ECMAScript-style module.")]
pub struct ADefaultExportCanOnlyBeUsedInAnEcmascriptStyleModule {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A default export must be at the top level of a file or module declaration.")]
pub struct ADefaultExportMustBeAtTheTopLevelOfAFileOrModuleDeclaration {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "'await' expressions are only allowed within async functions and at the top levels of modules."
)]
pub struct AwaitExpressionsAreOnlyAllowedWithinAsyncFunctionsAndAtTheTopLevelsOfModules {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Interface declaration cannot have 'implements' clause.")]
pub struct InterfaceDeclarationCannotHaveImplementsClause {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A rest element cannot have a property name.")]
pub struct ARestElementCannotHaveAPropertyName {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'delete' cannot be called on an identifier in strict mode.")]
pub struct DeleteCannotBeCalledOnAnIdentifierInStrictMode {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Invalid use of '{name}' in strict mode.")]
pub struct InvalidUseOf0InStrictMode {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Unterminated regular expression literal.")]
pub struct UnterminatedRegularExpressionLiteral {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("'{modifier}' modifier cannot appear on class elements of this kind.")]
pub struct ModifierCannotAppearOnClassElementsOfThisKind {
    #[label(primary)]
    pub span: Span,
    pub modifier: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'get' accessor cannot have parameters.")]
pub struct AGetAccessorCannotHaveParameters {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'set' accessor must have exactly one parameter.")]
pub struct ASetAccessorMustHaveExactlyOneParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Classes may not have a field named 'constructor'.")]
pub struct ClassesMayNotHaveAFieldNamedConstructor {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Only ambient modules can use quoted names.")]
pub struct OnlyAmbientModulesCanUseQuotedNames {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("{} Function type notation must be parenthesized when used in {} type.", {
    if self.is_fn_ty {
        "Function"
    } else {
        "Constructor"
    }
}, {
    if self.is_union_ty {
        "a union"
    } else {
        "an intersection"
    }
})]
pub struct FunctionTypeOrConstructorTypeNotationMustBeParenthesizedWhenUsedInAUnionTypeOrIntersectionType
{
    #[label(primary)]
    pub span: Span,
    pub is_fn_ty: bool,
    pub is_union_ty: bool,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("'{x}' modifier must precede '{y}' modifier.")]
pub struct XModifierMustPrecedeYModifier {
    #[label(primary)]
    pub span: Span,
    pub x: bolt_ts_ast::ModifierKind,
    pub y: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("An index signature cannot have a rest parameter.")]
pub struct AnIndexSignatureCannotHaveARestParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'yield' expression is only allowed in a generator body.")]
pub struct AYieldExpressionIsOnlyAllowedInAGeneratorBody {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Unexpected keyword or identifier.")]
pub struct UnexpectedKeywordOrIdentifier {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("An export assignment cannot have modifiers.")]
pub struct AnExportAssignmentCannotHaveModifiers {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A class declaration without the 'default' modifier must have a name.")]
pub struct AClassDeclarationWithoutTheDefaultModifierMustHaveAName {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("The left-hand side of an assignment expression must be a variable or a property access.")]
pub struct TheLeftHandSideOfAnAssignmentExpressionMustBeAVariableOrAPropertyAccess {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Duplicate label '{label}'.")]
pub struct DuplicateLabel {
    #[label(primary)]
    pub span: Span,
    pub label: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error(
    "Octal escape sequences and backreferences are not allowed in a character class. If this was intended as an escape sequence, use the syntax {code} instead."
)]
pub struct OctalEscapeSequencesAndBackreferencesAreNotAllowedInACharacterClassIfThisWasIntendedAsAnEscapeSequenceUseTheSyntaxXInstead
{
    #[label(primary)]
    pub span: Span,
    pub code: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Octal escape sequences are not allowed. Use the syntax '{code}'.")]
pub struct OctalEscapeSequencesAreNotAllowedUseTheSyntaxX {
    #[label(primary)]
    pub span: Span,
    pub code: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Decimal escape sequences and backreferences are not allowed in a character class.")]
pub struct DecimalEscapeSequencesAndBackreferencesAreNotAllowedInACharacterClass {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Escape sequence '{code}' is not allowed.")]
pub struct EscapeSequenceXIsNotAllowed {
    #[label(primary)]
    pub span: Span,
    pub code: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("An extended Unicode escape value must be between 0x0 and 0x10FFFF inclusive.")]
pub struct AnExtendedUnicodeEscapeValueMustBeBetween0x0And0x10FFFFInclusive {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Hexadecimal digit expected.")]
pub struct HexadecimalDigitExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A rest parameter cannot be optional.")]
pub struct ARestParameterCannotBeOptional {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A rest parameter cannot have an initializer.")]
pub struct ARestParameterCannotHaveAnInitializer {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A required parameter cannot follow an optional parameter.")]
pub struct ARequiredParameterCannotFollowAnOptionalParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A parameter property is only allowed in a constructor implementation.")]
pub struct AParameterPropertyIsOnlyAllowedInAConstructorImplementation {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("'constructor' cannot be used as a parameter property name.")]
pub struct ConstructorCannotBeUsedAsAParameterPropertyName {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("'{x}' is not a valid meta-property for keyword '{y}'. Did you mean '{z}'?")]
pub struct XIsNotAValidMetaPropertyForKeywordYDidYouMeanZ {
    #[label(primary)]
    pub span: Span,
    pub x: String,
    pub y: &'static str,
    pub z: &'static str,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'default' clause cannot appear more than once in a 'switch' statement.")]
pub struct ADefaultClauseCannotAppearMoreThanOnceInASwitchStatement {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("'let' is not allowed to be used as a name in 'let' or 'const' declarations.")]
pub struct LetIsNotAllowedToBeUsedAsANameInLetOrConstDeclarations {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'set' accessor cannot have an optional parameter.")]
pub struct ASetAccessorCannotHaveAnOptionalParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'set' accessor parameter cannot have an initializer.")]
pub struct ASetAccessorParameterCannotHaveAnInitializer {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'set' accessor cannot have rest parameter.")]
pub struct ASetAccessorCannotHaveRestParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("'super' may not use type arguments.")]
pub struct SuperMayNotUseTypeArguments {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Type parameters cannot appear on a constructor declaration.")]
pub struct TypeParametersCannotAppearOnAConstructorDeclaration {
    #[label(primary)]
    pub span: Span,
}
