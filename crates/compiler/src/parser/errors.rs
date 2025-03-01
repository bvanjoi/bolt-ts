use bolt_ts_ast::ModifierKind;
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
    pub kinds: Vec<ModifierKind>,
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
        write!(f, "{}", s)
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
    pub kind: ModifierKind,
}
