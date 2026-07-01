// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticVisibility2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

class Square {
    static sideLength;
    //~^ ERROR: Variable 'sideLength' implicitly has an 'any' type.
    constructor(sideLength: number) {
        this.sideLength = sideLength;
        //~^ ERROR: Property 'sideLength' does not exist on type 'Square<Square>'.
    }
}