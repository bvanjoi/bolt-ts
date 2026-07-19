// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/downlevelLetConst2.ts`, Apache-2.0 License

const a
//~^ ERROR: Declarations must be initialized.
//~| ERROR: Variable 'a' implicitly has an 'any' type.
