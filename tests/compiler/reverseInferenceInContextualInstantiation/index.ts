// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseInferenceInContextualInstantiation.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function compare<T>(a: T, b: T): number { return 0; }
var x: number[];
x.sort(compare); // Error, but shouldn't be
//~^ ERROR: Variable 'x' is used before being assigned.
