// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloadImplementationOfWrongName2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(x);
function bar() { }
//~^ ERROR: Function implementation name must be 'foo'.
function foo(x, y);
//~^ ERROR: Function implementation is missing or not immediately following the declaration.