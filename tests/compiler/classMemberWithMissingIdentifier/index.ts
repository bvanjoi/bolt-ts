// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classMemberWithMissingIdentifier.ts`, Apache-2.0 License

class C { 
    public {};
    //~^ ERROR: Identifier expected.
    //~| ERROR: Unexpected keyword or identifier.
    //~| ERROR: Expected '}'.
}
//~^ ERROR: Declaration or statement expected.

