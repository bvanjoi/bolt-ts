// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/numericIndexerConstraint4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    foo: number;
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
}

class B extends A {
    bar: string;
    //~^ ERROR: Property 'bar' has no initializer and is not definitely assigned in the constructor.
}

var x: {
    [idx: number]: A;
} = { 0: new B() }