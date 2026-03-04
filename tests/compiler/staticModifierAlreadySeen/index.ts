// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/staticModifierAlreadySeen.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    static static foo = 1;
    //~^ ERROR: Unexpected keyword or identifier.
    public static static bar() { }
    //~^ ERROR: Unexpected keyword or identifier.
    //~| ERROR: Duplicate identifier 'static'.
}