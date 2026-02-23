// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/instantiateTypeParameter.ts`, Apache-2.0 License

interface Foo<T> {
    var x: T<>;
    //~^ ERROR: Property or signature expected.
    //~| ERROR: Expected '}'.
    //~| ERROR: Type argument list cannot be empty.
    //~| ERROR: Cannot find name 'T'.
} //~ERROR: Declaration or statement expected. 
