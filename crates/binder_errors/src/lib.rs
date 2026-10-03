use bolt_ts_errors::DiagnosticExt;
use bolt_ts_errors::diag_ext;
use bolt_ts_errors::miette;
use bolt_ts_errors::miette::Diagnostic;
use bolt_ts_span::Span;

use thiserror::Error;

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Duplicate identifier '{name}'.")]
pub struct DuplicateIdentifier {
    #[label(primary)]
    pub span: Span,
    pub name: String,
    #[label("Previous definition here")]
    pub original_span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Enum declarations can only merge with namespace or other enum declarations.")]
pub struct EnumDeclarationsCanOnlyMergeWithNamespaceOrOtherEnumDeclarations {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("A module cannot have multiple default exports.")]
pub struct AModuleCannotHaveMultipleDefaultExports {
    #[label(primary)]
    pub span: Span,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Declaration name conflicts with built-in global identifier '{name}'.")]
pub struct DeclarationNameConflictsWithBuiltInGlobalIdentifierX {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error(
    "Code contained in a class is evaluated in JavaScript's strict mode, which does not allow this use of '{name}'. For more information, see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Strict_mode"
)]
pub struct CodeContainedInAClassIsEvaluatedInJavaScriptSStrictModeWhichDoesNotAllowThisUseOf0ForMoreInformationSeeHttpsColonSlashSlashdeveloperMozillaOrgSlashenUsSlashdocsSlashWebSlashJavaScriptSlashReferenceSlashStrictMode
{
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Invalid use of '{name}'. Modules are automatically in strict mode.")]
pub struct InvalidUseOfXModulesAreAutomaticallyInStrictMode {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}

#[derive(Error, Diagnostic, DiagnosticExt, Debug)]
#[error("Invalid use of '{name}' in strict mode.")]
pub struct InvalidUseOfXInStrictMode {
    #[label(primary)]
    pub span: Span,
    pub name: String,
}
