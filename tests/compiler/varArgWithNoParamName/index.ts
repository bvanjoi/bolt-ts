// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/varArgWithNoParamName.ts`, Apache-2.0 License

function t1(...) {}
//~^ ERROR: Identifier expected.
//~| ERROR: Rest parameter '' implicitly has an 'any[]' type.
