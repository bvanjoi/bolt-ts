// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/topLevelLambda2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class arrTest {
    test(arg1: number[]) {    }
    callTest() {
        // these two should give the same error
        this.test([1, 2, "hi", 5, ]);
        //~^ ERROR: Type 'string' is not assignable to type 'number'.
        this.test([1, 2, "hi", 5]); 
        //~^ ERROR: Type 'string' is not assignable to type 'number'.
    }
}
