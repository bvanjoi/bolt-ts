// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/letAsIdentifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var let = 10;
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
var a = 10;
let = 30;
//~^ ERROR: Identifier expected. 'let' is a reserved word in strict mode.
let
a;
//~^ ERROR: Duplicate identifier 'a'.
