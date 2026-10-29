// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleKeywordRepeatError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// "module.module { }" should raise a syntax error
module.module { }
//~^ ERROR: Unexpected keyword or identifier.
//~| ERROR: Cannot find name 'module'.
