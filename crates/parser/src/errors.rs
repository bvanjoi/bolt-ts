use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_errors::thiserror;
use bolt_ts_errors::thiserror::Error;
use bolt_ts_span::Span;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Syntax Error: Unexpected token ','")]
pub(super) struct ClassesCanOnlyExtendASingleClass {
    #[label(primary)]
    pub span: Span,
    #[label("Classes can only extend a single class.")]
    pub extra_extends: Option<Span>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type parameter list cannot be empty.")]
pub(super) struct TypeParameterListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Octal literals are not allowed.")]
#[diagnostic(help = "Use the syntax '{help_lit}'.")]
pub(super) struct OctalLiteralsAreNotAllowed {
    #[label(primary)]
    pub span: Span,
    pub help_lit: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Decimals with leading zeros are not allowed.")]
pub(super) struct DecimalsWithLeadingZerosAreNotAllowed {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Numeric separators are not allowed here.")]
pub(super) struct NumericSeparatorsAreNotAllowedHere {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Variable declaration list cannot be empty.")]
pub(super) struct VariableDeclarationListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A parameter property is only allowed in a constructor implementation.")]
pub(super) struct AParamPropIsOnlyAllowedInAConstructorImplementation {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{kind}")]
pub(super) struct MissingIdent {
    #[label(primary)]
    pub span: Span,
    pub kind: MissingIdentKind,
}

#[derive(Error, Diagnostic, Debug)]
pub(super) enum MissingIdentKind {
    #[error("Identifier expected.")]
    IdentifierExpected,
    #[error("Expression expected.")]
    ExpressionExpected,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Type argument list cannot be empty.")]
pub(super) struct TypeArgumentListCannotBeEmpty {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{} cannot be declared using a rest parameter.", {kinds.iter().map(|k| format!("'{k}'")).collect::<Vec<_>>().join(", ")})]
pub(super) struct AParameterPropertyCannotBeDeclaredUsingARestParameter {
    #[label(primary)]
    pub span: Span,
    pub kinds: Vec<bolt_ts_ast::ModifierKind>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected '{x}'.")]
pub(super) struct ExpectX {
    #[label(primary)]
    pub span: Span,
    pub x: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum ClauseKind {
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
pub(super) struct ClauseAlreadySeen {
    #[label(primary)]
    pub span: Span,
    pub kind: ClauseKind,
    #[label("It was first defined here.")]
    pub origin: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'extends' clause must precede 'implements' clause.")]
pub(super) struct ExtendsClauseMustPrecedeImplementsClause {
    #[label(primary)]
    pub extends_span: Span,
    #[label("'implements' clause defined here.")]
    pub implements_span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{expected}' expected.")]
pub(super) struct KindExpected {
    #[label(primary)]
    pub span: Span,
    pub expected: String,
    #[related]
    pub related: Option<ExpectedToFindAToMatchTheBTokenHere>,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected to find a '{expected}' to match the '{found}' token here.")]
#[diagnostic(severity(Advice))]
pub(super) struct ExpectedToFindAToMatchTheBTokenHere {
    #[label(primary)]
    pub span: Span,
    pub expected: String,
    pub found: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Argument expression expected.")]
pub(super) struct ArgumentExpressionExpected {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature must have a type annotation.")]
pub(super) struct AnIndexSignatureMustHaveATypeAnnotation {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{kind}' modifier cannot appear on a parameter.")]
pub(super) struct ModifierCannotAppearOnAParameter {
    #[label(primary)]
    pub span: Span,
    pub kind: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An implementation cannot be declared in ambient contexts.")]
pub(super) struct AnImplementationCannotBeDeclaredInAmbientContexts {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An element access expression should take an argument.")]
pub(super) struct AnElementAccessExpressionShouldTakeAnArgument {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Declaration or statement expected.")]
pub(super) struct DeclarationOrStatementExpected {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An object member cannot be declared optional.")]
pub(super) struct AnObjectMemberCannotBeDeclaredOptional {
    #[label(primary)]
    pub(super) span: Span,
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
pub(super) struct AContinueOrBreakStatementCanOnlyBeUsedWithinAnEnclosingIterationStatement {
    #[label(primary)]
    pub(super) span: Span,
    pub(super) is_continue: bool,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("{} property cannot have an initializer.", {
    if self.interface {
        "An interface"
    } else {
        "A type literal"
    }
})]
pub(super) struct AnInterfaceOrTypeLitPropertyCannotHaveAnInitializer {
    #[label(primary)]
    pub(super) span: Span,
    pub(super) interface: bool,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("An enum member cannot have a numeric name.")]
pub(super) struct AnEnumMemberCannotHaveANumericName {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Accessibility modifier already seen.")]
pub(super) struct AccessibilityModifierAlreadySeen {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Unexpected token. Did you mean `{{'}}'}}` or `&rbrace;`?")]
pub(super) struct UnexpectedTokenDidYouMeanOrRBrace {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Unexpected token. Did you mean `{{'>'}}` or `&gt;`?")]
pub(super) struct UnexpectedTokenDidYouMeanOrGt {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Tagged template expressions are not permitted in an optional chain.")]
pub(super) struct TaggedTemplateExpressionsAreNotPermittedInAnOptionalChain {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Expected corresponding closing tag for JSX fragment.")]
pub(super) struct ExpectedCorrespondingClosingTagForJsxFragment {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Unterminated string literal.")]
pub(super) struct UnterminatedStringLiteral {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Unterminated template literal.")]
pub(super) struct UnterminatedTemplateLiteral {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'{{' or JSX element expected.")]
pub(super) struct OrJsxElementExpected {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Expected corresponding JSX closing tag for '{opening_tag_name}'.")]
pub(super) struct ExpectedCorrespondingJsxClosingTagForOpeningTagName {
    #[label(primary)]
    pub(super) span: Span,
    pub(super) opening_tag_name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Invalid character.")]
pub(super) struct InvalidCharacter {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Unicode escape sequence cannot appear here.")]
pub(super) struct UnicodeEscapeSequenceCannotAppearHere {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Digit expected.")]
pub(super) struct DigitExpected {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Keywords cannot contain escape characters.")]
pub(super) struct KeywordsCannotContainEscapeCharacters {
    #[label(primary)]
    pub(super) span: Span,
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
pub(super) struct ArgumentsCannotBeReferenced {
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
#[error("Property or signature expected.")]
pub(super) struct PropertyOrSignatureExpected {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A 'return' statement can only be used within a function body.")]
pub(super) struct AReturnStatementCanOnlyBeUsedWithinAFunctionBody {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Modifier cannot be used here.")]
pub(super) struct ModifierCannotBeUsedHere {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A class member cannot have the '{modifier}' keyword.")]
pub(super) struct AClassMemberCannotHaveTheModifierKeyword {
    #[label(primary)]
    pub(super) span: Span,
    pub(super) modifier: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Identifier expected. '{identifier}' is a reserved word in strict mode.")]
pub(super) struct IdentifierExpectedXIsAReservedWordInStrictMode {
    #[label(primary)]
    pub(super) span: Span,
    pub(super) identifier: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature cannot have a trailing comma.")]
pub(super) struct AnIndexSignatureCannotHaveATrailingComma {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature must have exactly one parameter.")]
pub(super) struct AnIndexSignatureMustHaveExactlyOneParameter {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Declarations must be initialized.")]
pub(super) struct DeclarationsMustBeInitialized {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("The left-hand side of a 'for...{}' statement cannot use a type annotation.", {
    if self.is_for_in {
        "in"
    } else {
        "of"
    }
})]
pub(super) struct TheLeftHandSideOfAForInOfStatementCannotUseATypeAnnotation {
    #[label(primary)]
    pub(super) span: Span,
    pub(super) is_for_in: bool,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Line break not permitted here.")]
pub(super) struct LineBreakNotPermittedHere {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature parameter cannot have an accessibility modifier.")]
pub(super) struct AnIndexSignatureParameterCannotHaveAnAccessibilityModifier {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature parameter cannot have a question mark.")]
pub(super) struct AnIndexSignatureParameterCannotHaveAQuestionMark {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("An index signature parameter cannot have an initializer.")]
pub(super) struct AnIndexSignatureParameterCannotHaveAnInitializer {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Jump target cannot cross function boundary.")]
pub(super) struct JumpTargetCannotCrossFunctionBoundary {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A default export can only be used in an ECMAScript-style module.")]
pub(super) struct ADefaultExportCanOnlyBeUsedInAnEcmascriptStyleModule {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A default export must be at the top level of a file or module declaration.")]
pub(super) struct ADefaultExportMustBeAtTheTopLevelOfAFileOrModuleDeclaration {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "'await' expressions are only allowed within async functions and at the top levels of modules."
)]
pub(super) struct AwaitExpressionsAreOnlyAllowedWithinAsyncFunctionsAndAtTheTopLevelsOfModules {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Interface declaration cannot have 'implements' clause.")]
pub(super) struct InterfaceDeclarationCannotHaveImplementsClause {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A rest element cannot have a property name.")]
pub(super) struct ARestElementCannotHaveAPropertyName {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("'delete' cannot be called on an identifier in strict mode.")]
pub(super) struct DeleteCannotBeCalledOnAnIdentifierInStrictMode {
    #[label(primary)]
    pub(super) span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Invalid use of '{name}' in strict mode.")]
pub(super) struct InvalidUseOf0InStrictMode {
    #[label(primary)]
    pub(super) span: Span,
    pub(super) name: String,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Unterminated regular expression literal.")]
pub(super) struct UnterminatedRegularExpressionLiteral {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Initializers are not allowed in ambient contexts.")]
pub(super) struct InitializersAreNotAllowedInAmbientContexts {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("'{modifier}' modifier cannot appear on class elements of this kind.")]
pub(super) struct ModifierCannotAppearOnClassElementsOfThisKind {
    #[label(primary)]
    pub span: Span,
    pub modifier: bolt_ts_ast::ModifierKind,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'get' accessor cannot have parameters.")]
pub(super) struct AGetAccessorCannotHaveParameters {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("A 'set' accessor must have exactly one parameter.")]
pub(super) struct ASetAccessorMustHaveExactlyOneParameter {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, Debug, DiagnosticExt)]
#[error("Classes may not have a field named 'constructor'.")]
pub(super) struct ClassesMayNotHaveAFieldNamedConstructor {
    #[label(primary)]
    pub span: Span,
}
