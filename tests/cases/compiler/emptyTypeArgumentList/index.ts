// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/emptyTypeArgumentList.ts`, Apache-2.0 License

function foo<T>() { }
foo<>();
//~^ ERROR: Type argument list cannot be empty.

function noParams() {}
noParams<>();
//~^ ERROR: Type argument list cannot be empty.
