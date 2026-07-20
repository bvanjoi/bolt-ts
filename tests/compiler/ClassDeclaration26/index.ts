// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/ClassDeclaration26.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    public const var export foo = 10;
    //~^ ERROR: A class member cannot have the 'const' keyword.
    //~| ERROR: Unexpected keyword or identifier.
    //~| ERROR: 'export' modifier cannot appear on class elements of this kind.

    var constructor() { }
    //~^ ERROR: Expected '}'.
    //~| ERROR: Expected ','.
    //~| ERROR: Expression expected.
}
//~^ ERROR: Declaration or statement expected.
