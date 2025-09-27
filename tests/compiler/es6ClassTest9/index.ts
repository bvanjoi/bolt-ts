// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/es6ClassTest9.ts`, Apache-2.0 License

declare class foo();
//~^ ERROR: Expected '{'.
//~| ERROR: Identifier expected.
function foo() {}
//~ ERROR: Expected '}'.