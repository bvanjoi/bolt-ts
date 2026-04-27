// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralMemberWithoutBlock1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var v = { foo(); }
//~^ ERROR: Expected '{'.
//~ ERROR: '}' expected.