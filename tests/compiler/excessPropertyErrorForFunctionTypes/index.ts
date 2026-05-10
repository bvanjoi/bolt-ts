// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/excessPropertyErrorForFunctionTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type FunctionType = () => any;
type DoesntWork = { a: number, c: number } | FunctionType;

let doesntWork: DoesntWork = { a: 1, c: 2, d: 3 }
//~^ ERROR: Object literal may only specify known properties, and 'd' does not exist in type 'DoesntWork'.