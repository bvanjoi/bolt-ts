// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classMemberWithMissingIdentifier2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C { 
    public {[name:string]:VariableDeclaration};
    //~^ ERROR: Identifier expected.
    //~| ERROR: Unexpected keyword or identifier.
    //~| ERROR: Expected '}'.
    //~| ERROR: Expected ','.
    //~| ERROR: Declaration or statement expected.
    //~| ERROR: Cannot find name 'string'.
    //~| ERROR: Cannot find name 'VariableDeclaration'.
}   //~ERROR: Declaration or statement expected.