// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parserUnparsedTokenCrash2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export = } x = ( y = z ==== 'function') {
//~^ ERROR: Expression expected.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Expression expected.
//~| ERROR: Expected ')'.
//~| ERROR: The left-hand side of an assignment expression must be a variable or a property access.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name ''.
//~| ERROR: Cannot find name 'x'.
//~| ERROR: Cannot find name 'y'.
//~| ERROR: Cannot find name 'z'.
//~| ERROR: Type 'string' is not assignable to type 'boolean'.
//~| ERROR: Unexpected keyword or identifier.
//~ ERROR: '}' expected.