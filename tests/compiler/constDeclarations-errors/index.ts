// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations-errors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// error, missing intialicer
const c1;
//~^ ERROR: Declarations must be initialized.
const c2: number;
//~^ ERROR: Declarations must be initialized.
const c3, c4, c5 :string, c6;  // error, missing initialicer
//~^ ERROR: Declarations must be initialized.
//~| ERROR: Declarations must be initialized.
//~| ERROR: Declarations must be initialized.
//~| ERROR: Declarations must be initialized.

for(const c in {}) { }

// error, assigning to a const
for(const c8 = 0; c8 < 1; c8++) { }
//~^ ERROR: Cannot assign to 'c8' because it is a constant.

// error, can not be unintalized
for(const c9; c9 < 1;) { }
//~^ ERROR: Declarations must be initialized.

// error, can not be unintalized
for(const c10 = 0, c11; c10 < 1;) { }
//~^ ERROR: Declarations must be initialized.
