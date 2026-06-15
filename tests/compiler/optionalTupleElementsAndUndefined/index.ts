// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalTupleElementsAndUndefined.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type UnNullify<T> = { [K in keyof T]: NonNullable<T[K]> };

type Foo = UnNullify<[a: 1, b?: 2 | undefined]>;

type Test = [a: 1, b?: 2] extends Foo ? true : false;  // true

// Types in the following declarations should be identical

var v: [1, 2?];
var v: [1, (2 | undefined)?];
var v: [a: 1, b?: 2];
var v: [a: 1, b?: 2 | undefined];
var v: UnNullify<[1, 2?]>;
var v: UnNullify<[1, (2 | undefined)?]>;
var v: UnNullify<[a: 1, b?: 2]>;
var v: UnNullify<[a: 1, b?: 2 | undefined]>;
