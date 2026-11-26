// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationOptionalParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs

function F(p?) { }
//~^ ERROR: The '?' modifier can only be used in TypeScript files.
//~| ERROR: Parameter 'p' implicitly has an 'any' type.
