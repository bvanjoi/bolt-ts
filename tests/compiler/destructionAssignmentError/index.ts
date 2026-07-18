// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructionAssignmentError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function fn(): { a: 1, b: 2 }
let a: number;
let b: number;

({ a, b } = fn());
{ a, b } = fn();
//~^ ERROR: Declaration or statement expected.
//~| ERROR:  Left side of comma operator is unused and has no side effects.

({ a, b } =
fn());

{ a, b }
= fn();
//~^ ERROR: Declaration or statement expected.
//~^^^ ERROR:  Left side of comma operator is unused and has no side effects.
