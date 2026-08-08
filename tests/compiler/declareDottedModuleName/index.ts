// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declareDottedModuleName.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace M {
    namespace P.Q { } // This shouldnt be emitted
}

namespace M {
    export namespace R.S { }  //This should be emitted
}

namespace T.U { // This needs to be emitted
}
