// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalAnyCheckTypePicksBothBranches.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type T = any extends number ? 1 : 0;
let x: T;
x = 1;
x = 0; // not an error

type U = [any] extends [number] ? 1 : 0;
let y: U;
y = 1;
y = 0; // error
//~^ ERROR: Type '0' is not assignable to type '1'.