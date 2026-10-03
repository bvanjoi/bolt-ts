// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferentialTypingWithFunctionTypeZip.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var pair: <T, S>(x: T) => (y: S) => { x: T; y: S; }
var zipWith: <T, S, U>(a: T[], b: S[], f: (x: T) => (y: S) => U) => U[];
var result = zipWith([1, 2], ['a', 'b'], pair);
//~^ ERROR: Variable 'zipWith' is used before being assigned.
//~| ERROR: Variable 'zipWith' is used before being assigned.
//~| ERROR: Variable 'pair' is used before being assigned.
//~| ERROR: Variable 'pair' is used before being assigned.
//~| ERROR: Variable 'pair' is used before being assigned.
//~| ERROR: Variable 'pair' is used before being assigned.
var i = result[0].x; // number