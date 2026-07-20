// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCloduleInModule2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    export class B<T> {
        foo() { }
        static bar() { }
    }
}

namespace A {
    export namespace B {
        export var x = 1;
    }
}

var b: A.B;
//~^ ERROR: Generic type 'A.B<T>' requires 1 type argument.
b.foo();
