// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileForInterfaceWithOptionalFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

interface I {
    foo? (x?);
    foo2? (x?: number): number;
}