// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superWithTypeArgument.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    
}

class D<T> extends C {
    constructor() {
        super<T>();
        //~^ ERROR: 'super' may not use type arguments.
    }
}
