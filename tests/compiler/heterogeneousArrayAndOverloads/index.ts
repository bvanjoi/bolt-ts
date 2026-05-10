// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/heterogeneousArrayAndOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false

class arrTest {
    test(arg1: number[]);
    test(arg1: string[]);
    test(arg1: any[]) { }
    callTest() {
        this.test([1, 2, 3, 5]);
        this.test(["hi"]);
        this.test([]);
        this.test([1, 2, "hi", 5]); // Error
        //~^ ERROR: No overload matches this call.
    }
}