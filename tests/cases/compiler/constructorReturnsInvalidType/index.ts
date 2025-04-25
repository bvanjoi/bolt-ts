// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorReturnsInvalidType.ts`, Apache-2.0 License

class X {
    constructor() {
        return 1;
        //~^ ERROR: Type 'number' is not assignable to type 'X'.
    }
    foo() { }
}
 
var x = new X();
