// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/ClassDeclaration21.ts`, Apache-2.0 License

class C {
    0();
    //~^ ERROR: Function implementation is missing or not immediately following the declaration.
    1() { }
}