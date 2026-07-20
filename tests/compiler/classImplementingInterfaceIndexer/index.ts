// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classImplementingInterfaceIndexer.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
    [index: string]: { prop }
}
class A implements I {
    [index: string]: { prop }
}
