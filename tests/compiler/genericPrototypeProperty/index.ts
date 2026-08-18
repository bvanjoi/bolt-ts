// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericPrototypeProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> {
    x: T;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
    foo(x: T): T { return null; }
    //~^ ERROR: Type 'null' is not assignable to type 'T'.
}

var r = C.prototype;
// should be any
var r2 = r.x
var r3 = r.foo(null);
