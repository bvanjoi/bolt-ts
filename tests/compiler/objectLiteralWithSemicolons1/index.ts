// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/objectLiteralWithSemicolons1.ts`, Apache-2.0 License

var v = { a; b; c }
//~^ ERROR: Expected ','.
//~| ERROR: Expected ','.
//~| ERROR: Cannot find name 'a'.
//~| ERROR: Cannot find name 'b'.
//~| ERROR: Cannot find name 'c'.
