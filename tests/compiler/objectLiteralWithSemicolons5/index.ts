// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralWithSemicolons5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var v = { foo() { }; a: b; get baz() { }; }
//~^ ERROR: Cannot find name 'b'.
//~| ERROR: Expected ','.
//~| ERROR: Expected ','.
//~| ERROR: Expected ','.
