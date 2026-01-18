// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/emitCapturingThisInTupleDestructuring1.ts`, Apache-2.0 License

declare function wrapper(x: any);
wrapper((array: [any]) => {
    [this.test, this.test1, this.test2] = array;  // even though there is a compiler error, we should still emit lexical capture for "this"
    //~^ ERROR: Tuple type '[any]' of length '1' has no element at index '1'.
    //~| ERROR: Tuple type '[any]' of length '1' has no element at index '2'.
});