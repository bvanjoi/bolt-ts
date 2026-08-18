// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileForInterfaceWithRestParams.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

interface I {
    foo(...x): typeof x;
    foo2(a: number, ...x): typeof x;
    foo3(b: string, ...x: string[]): typeof x;
}