// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedNamespaceInNamespace.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noEmit
//@compiler-options: noUnusedLocals

namespace A {
    namespace B {}
    //~^ ERROR: 'B' is declared but its value is never read.
    export namespace C {}
}