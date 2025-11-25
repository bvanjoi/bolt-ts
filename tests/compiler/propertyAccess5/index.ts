// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/propertyAccess5.ts`, Apache-2.0 License

//@ run-fail

undefined.toBAZ();
//~^ ERROR: The value 'undefined' cannot be used here.