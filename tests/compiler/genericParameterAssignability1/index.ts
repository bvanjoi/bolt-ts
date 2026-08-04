// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericParameterAssignability1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f<T>(x: T): T { return null; }
//~^ ERROR: Type 'null' is not assignable to type 'T'.
var r = <T>(x: T) => x;
r = f; // should be allowed