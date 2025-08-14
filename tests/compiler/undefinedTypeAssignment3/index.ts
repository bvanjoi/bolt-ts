// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/undefinedTypeAssignment3.ts`, Apache-2.0 License

var undefined = null;
//~^ ERROR: Declaration name conflicts with built-in global identifier 'undefined'.
