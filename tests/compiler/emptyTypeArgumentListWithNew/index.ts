// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/emptyTypeArgumentListWithNew.ts`, Apache-2.0 License

class foo<T> { }
new foo<>();
//~^ ERROR: Type argument list cannot be empty.

class noParams {}
new noParams<>();
//~^ ERROR: Type argument list cannot be empty.
