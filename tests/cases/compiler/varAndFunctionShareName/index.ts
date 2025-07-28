// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/varAndFunctionShareName.ts`, Apache-2.0 License

var myFn;
function myFn(): any { }
//~^ ERROR: Duplicate identifier 'myFn'.