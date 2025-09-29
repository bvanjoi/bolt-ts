// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/emptyGenericParamList.ts`, Apache-2.0 License

class I<T> {}
var x: I<>;
//~^ ERROR: Type argument list cannot be empty.
//~| ERROR: Generic type 'I<T>' requires 1 type argument.