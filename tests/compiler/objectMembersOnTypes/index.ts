// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectMembersOnTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I {}
class AAA implements I { }
var x: number;
x.toString();
//~^ ERROR: Variable 'x' is used before being assigned.
var i: I;
i.toString(); // used to be an error
//~^ ERROR: Variable 'i' is used before being assigned.
var c: AAA;
c.toString(); // used to be an error
//~^ ERROR: Variable 'c' is used before being assigned.
