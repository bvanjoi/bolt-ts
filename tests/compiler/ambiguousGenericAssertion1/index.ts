// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/ambiguousGenericAssertion1.ts`, Apache-2.0 License

function f<T>(x: T): T { return null; }
var r = <T>(x: T) => x;
var r2 = < <T>(x: T) => T>f; // valid
var r3 = <<T>(x: T) => T>f; // ambiguous, appears to the parser as a << operation
//~^ ERROR: Expression expected.
//~| ERROR: Expected ')'.
//~| ERROR: Expected ','.
//~| ERROR: Expected ','.
//~| ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'x'.


