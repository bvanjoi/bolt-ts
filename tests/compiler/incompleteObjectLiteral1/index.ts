// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/incompleteObjectLiteral1.ts`, Apache-2.0 License

var tt = { aa; }
//~^ ERROR: Expected ','.
//~| ERROR: Cannot find name 'aa'.
var x = tt;