// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/methodChainError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Builder {
    notMethod: string
    //~^ ERROR: Property 'notMethod' has no initializer and is not definitely assigned in the constructor.
    method(param: string): Builder {
        return this;
    }
}

new Builder()
    .method("a")
    .method()   //~ERROR: Expected 1 arguments, but got 0.
    .method("a");

    
new Builder()
    .method("a")
    .notMethod()  //~ERROR: This expression is not callable.
    .method("a");

class A {
    a(a: string) {}
}

(new A()).a();
//~^ ERROR: Expected 1 arguments, but got 0.