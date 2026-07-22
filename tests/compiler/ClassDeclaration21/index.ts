// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ClassDeclaration21.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    0();
    1() { }
    //~^ ERROR: Function implementation name must be '0'.
}
