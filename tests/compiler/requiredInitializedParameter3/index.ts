// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/requiredInitializedParameter3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

interface I1 {
    method();
}

class C1 implements I1 {
    method(a = 0, b?) { }
}