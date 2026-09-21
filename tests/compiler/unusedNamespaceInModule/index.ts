// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedNamespaceInModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace A {
    namespace B {}
    //~^ ERROR: 'B' is declared but its value is never read.
    export namespace C {}
}