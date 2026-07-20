// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restParamAsOptional.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f(...x?) { }
//~^ ERROR: A rest parameter cannot be optional.
function f2(...x = []) { }
//~^ ERROR: A rest parameter cannot have an initializer.