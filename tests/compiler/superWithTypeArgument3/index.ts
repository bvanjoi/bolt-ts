// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superWithTypeArgument3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C<T> {
    foo: T;
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
    bar<U>(x: U) { }
}

class D<T> extends C<T> {
    constructor() {
        super<T>();
        //~^ ERROR: 'super' may not use type arguments.
    }
    bar() {
        super.bar<T>(null);
        //~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'T'.
    }
}