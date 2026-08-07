// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileOptionalInterfaceMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

interface X {
    f? <T>();
}
