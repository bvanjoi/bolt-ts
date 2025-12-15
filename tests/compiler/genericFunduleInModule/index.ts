// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericFunduleInModule.ts`, Apache-2.0 License

namespace A {
    export function B<T>(x: T) { return x; }
    export namespace B {
        export var x = 1;
    }
}

var b: A.B;
//~^ ERROR: 'A.B' refers to a value, but is being used as a type here. Did you mean 'typeof A.B'
A.B(1);