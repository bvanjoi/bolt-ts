// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/requiredInitializedParameter4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

class C1 {
    method(a = 0, b) { }
}