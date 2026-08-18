// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/compositeGenericFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
function f<T>(value: T) { return value; };

function h<R>(func: (x: number) => R): R { return null; }
//~^ ERROR: Type 'null' is not assignable to type 'R'.

var z: number = h<number>(f);
var z: number = h(f);