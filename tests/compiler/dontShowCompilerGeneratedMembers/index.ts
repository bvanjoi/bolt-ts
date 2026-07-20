// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/dontShowCompilerGeneratedMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var f: {
    x: number;
    <-
    //~^ ERROR: Expected '>'.
    //~| ERROR: Type parameter list cannot be empty.
    //~| ERROR: Expected '('.
    //~| ERROR: Property or signature expected.
    //~| ERROR: Expected '}'.
    //~| ERROR: Expected ','.
};
//~^ ERROR: Expression expected.
//~| ERROR: Declaration or statement expected.