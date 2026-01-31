// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classImplementingInterfaceIndexer.ts`, Apache-2.0 License

interface I {
    [index: string]: { prop }
}
class A implements I {
    [index: string]: { prop }
}