// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/callSignaturesShouldBeResolvedBeforeSpecialization.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface I1<T> {
    (value: T): void;
    field1: I1<boolean>;
}

function foo() {
    var test!: I1<string>;
    test("expects boolean instead of string"); // should not error - "test" should not expect a boolean
    test(true); // should error - string expected
    //~^ ERROR: Argument of type 'boolean' is not assignable to parameter of type 'string'.
}