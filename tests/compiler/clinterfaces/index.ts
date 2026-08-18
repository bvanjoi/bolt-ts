// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/clinterfaces.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

namespace M {
    class C { }
    interface C { }
    interface D { }
    class D { }
}

interface Foo<T> {
    a: string;
}

class Foo<T>{
    b: number;
    //~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
}

class Bar<T>{
    b: number;
    //~^ ERROR: Property 'b' has no initializer and is not definitely assigned in the constructor.
}

interface Bar<T> {
    a: string;
}

export = Foo;