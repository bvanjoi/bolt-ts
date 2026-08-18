// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignToExistingClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Test {
    class Mocked {
        myProp: string;
        //~^ ERROR: Property 'myProp' has no initializer and is not definitely assigned in the constructor.
    }

    class Tester {
        willThrowError() {
            Mocked = Mocked || function () { // => Error: Invalid left-hand side of assignment expression.
                return { myProp: "test" };
            };
            //~^^^ ERROR: Cannot assign to 'Mocked' because it is a class.
            //~| ERROR: Cannot assign to 'Mocked' because it is a class.
            //~| ERROR: Cannot assign to 'Mocked' because it is a class.
        }
    }
 
}