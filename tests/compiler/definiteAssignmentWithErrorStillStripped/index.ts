// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/definiteAssignmentWithErrorStillStripped.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015
//@compiler-options: useDefineForClassFields

class C {
    p!;
    //~^ ERROR: Declarations with definite assignment assertions must also have type annotations.
    static a0!;
    //~^ ERROR: Declarations with definite assignment assertions must also have type annotations.
    g1! = 123;
    //~^ ERROR: Declarations with initializers cannot also have definite assignment assertions.
    g2!: number;
}
