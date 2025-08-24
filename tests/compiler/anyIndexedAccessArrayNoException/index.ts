// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/anyIndexedAccessArrayNoException.ts`, Apache-2.0 License

var x: any[[]];
//~^ ERROR: Type '[]' cannot be used as an index type.
