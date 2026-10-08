// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeofClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class K {
    foo: number;
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
    static bar: string;
}

declare var k1: K;
k1.foo;
k1.bar;
//~^ ERROR: Property 'bar' does not exist on type 'K'.
declare var k2: typeof K;
k2.foo;
//~^ ERROR: Property 'foo' does not exist on type 'typeof K'.
k2.bar;