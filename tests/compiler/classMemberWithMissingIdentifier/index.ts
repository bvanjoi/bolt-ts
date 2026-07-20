// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classMemberWithMissingIdentifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C { 
    public {};
    //~^ ERROR: Identifier expected.
    //~| ERROR: Unexpected keyword or identifier.
    //~| ERROR: Expected '}'.
}
//~^ ERROR: Declaration or statement expected.

