// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/interfaceOnly.ts`, Apache-2.0 License

//@compiler-options: declaration

interface foo {
    foo();
    f2 (f: ()=> void);
}