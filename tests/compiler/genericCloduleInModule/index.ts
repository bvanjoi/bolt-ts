// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCloduleInModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    export class B<T> {
        foo() { }
        static bar() { }
    }
    export namespace B {
        export var x = 1;
    }
}

var b: A.B<number>;
b.foo();
//~^ ERROR: Variable 'b' is used before being assigned.