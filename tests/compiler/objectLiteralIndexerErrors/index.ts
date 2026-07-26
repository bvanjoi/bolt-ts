// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralIndexerErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    x: number;
}

interface B extends A {
    y: string;
}

declare var a: A;
declare var b: B;
var c: any;

var o1: { [s: string]: A;[n: number]: B; } = { x: b, 0: a }; // both indexers are A
//~^ ERROR: Property 'y' is missing.
o1 = { x: c, 0: a }; // string indexer is any, number indexer is A
//~^ ERROR: Property 'y' is missing.
