// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidReferenceSyntax1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

/// <reference path="missingquote.ts />
//~^ ERROR: Unterminated string literal.
class C {

}