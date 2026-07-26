// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringTuple.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare var tuple: [boolean, number, ...string[]];

const [a, b, c, ...rest] = tuple;

declare var receiver: typeof tuple;

[...receiver] = tuple;

// Repros from #32140

const [oops1] = [1, 2, 3].reduce((accu, el) => accu.concat(el), []);
//~^ ERROR: Type 'number' must have a '[Symbol.iterator]()' method that returns an iterator.
//~| ERROR: No overload matches this call.
//~| ERROR: No overload matches this call.

const [oops2] = [1, 2, 3].reduce((acc: number[], e) => acc.concat(e), []);
