// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/constraints0.ts`, Apache-2.0 License

interface A {
	a: number;
}

interface B {
	b: string;
}

interface C<T extends A> {
    x: T;
}

var v1: C<A>; // should work
var v2: C<B>; // should not work
//~^ ERROR: Property 'a' is missing.

var y = v1.x.a; // 'a' should be of type 'number'
var y1: number = v1.x.a;