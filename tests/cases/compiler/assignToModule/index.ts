// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/assignToModule.ts`, Apache-2.0 License

module A {}
A = undefined;
//~^ ERROR: Cannot find name 'A'.