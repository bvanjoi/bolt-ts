// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/incompleteDottedExpressionAtEOF.ts`, Apache-2.0 License

var p2 = window.
//~^ ERROR: Cannot find name 'window'.
//~ ERROR: Identifier expected.