// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateTypeParameters1.ts`, Apache-2.0 License

function A<X, X>() { }
//~^ ERROR: Duplicate identifier 'X'.