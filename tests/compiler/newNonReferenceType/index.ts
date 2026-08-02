// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newNonReferenceType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var a = new any();
//~^ ERROR: Cannot find name 'any'.
var b = new boolean(); // error
//~^ ERROR: Cannot find name 'boolean'.
