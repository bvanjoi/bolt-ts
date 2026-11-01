// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportEqualsClassRedeclarationError.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

class SomeClass {
    static get someProp(): number {
        return 0;
    }

    static set someProp(value: number) {}
    static set someProp(value: number) {}
    //~^ ERROR: Duplicate identifier 'someProp'.
    //~| ERROR: Duplicate identifier 'someProp'.
}
export = SomeClass;