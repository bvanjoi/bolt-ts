// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/emitCapturingThisInTupleDestructuring2.ts`, Apache-2.0 License

var array1: [number, number] = [1, 2];

class B {
    test: number;
    test1: any;
    test2: any;
    method() {
        () => [this.test, this.test1, this.test2] = array1; // even though there is a compiler error, we should still emit lexical capture for "this" 
        //~^ ERROR: Tuple type '[number, number]' of length '2' has no element at index '2'.
    }
}
