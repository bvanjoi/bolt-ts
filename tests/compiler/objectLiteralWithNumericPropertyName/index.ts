// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/objectLiteralWithNumericPropertyName.ts`, Apache-2.0 License

interface A {
    0: string;
}
var x: A = {
    0: 3
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
};
