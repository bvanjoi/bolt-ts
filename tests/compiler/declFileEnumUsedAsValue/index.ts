// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileEnumUsedAsValue.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

enum e {
    a,
    b,
    c
}
var x = e;