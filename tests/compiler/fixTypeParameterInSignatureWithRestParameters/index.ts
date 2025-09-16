// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/fixTypeParameterInSignatureWithRestParameters.ts`, Apache-2.0 License

function bar<T>(item1: T, item2: T) { }
bar(1, ""); // Should be ok
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.