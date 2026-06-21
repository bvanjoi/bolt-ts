// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superWithTypeArgument2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C<T> {
    foo: T;
}

class D<T> extends C<T> {
    constructor(x) {
        super<T>(x);
        //~^ ERROR: 'super' may not use type arguments.
        //~| ERROR: Expected 0 arguments, but got 1.
    }
}
