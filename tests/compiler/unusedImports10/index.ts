// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedImports10.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace A {
    export class Calculator {
        public handelChar() {
        }
    }
}

namespace B {
    import a = A;
    //~^ ERROR: 'a' is declared but its value is never read.
}