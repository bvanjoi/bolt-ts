// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralIndexers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    x: number;
}

interface B extends A {
    y: string;
}

var a: A;
var b: B;
var c: any;

var o1: { [s: string]: A;[n: number]: B; } = { x: a, 0: b }; // string indexer is A, number indexer is B
//~^ ERROR: Variable 'a' is used before being assigned.
//~| ERROR: Variable 'b' is used before being assigned.
o1 = { x: b, 0: c }; // both indexers are any
//~^ ERROR: Variable 'b' is used before being assigned.
o1 = { x: c, 0: b }; // string indexer is any, number indexer is B
//~^ ERROR: Variable 'b' is used before being assigned.
