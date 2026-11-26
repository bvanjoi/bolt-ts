// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/jsFileCompilationReturnTypeSyntaxOfFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowJs

function F(): number { }
//~^ ERROR: Type annotations can only be used in TypeScript files.
//~| ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.