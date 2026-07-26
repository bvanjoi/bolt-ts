// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fatarrowfunctionsOptionalArgsErrors2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var tt1 = (a, (b, c)) => a+b+c;
//~^ ERROR: Cannot find name 'a'.
//~| ERROR: Cannot find name 'b'.
//~| ERROR: Cannot find name 'c'.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'a'.
//~| ERROR: Cannot find name 'b'.
//~| ERROR: Cannot find name 'c'.
var tt2 = ((a), b, c) => a+b+c;
//~^ ERROR: Cannot find name 'a'.
//~| ERROR: Cannot find name 'b'.
//~| ERROR: Cannot find name 'c'.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Left side of comma operator is unused and has no side effects.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'a'.
//~| ERROR: Cannot find name 'b'.
//~| ERROR: Cannot find name 'c'.
var tt3 = ((a)) => a;
//~^ ERROR: Cannot find name 'a'.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'a'.
