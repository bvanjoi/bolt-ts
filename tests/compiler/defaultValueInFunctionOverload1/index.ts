// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultValueInFunctionOverload1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(x: string = '');
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
function foo(x = '') { }