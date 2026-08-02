// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileModuleContinuation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace A.C {
    export interface Z {
    }
}

namespace A.B.C {
    export class W implements A.C.Z {
    }
}