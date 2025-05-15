


// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infinitelyExpandingTypes4.ts`, Apache-2.0 License
// ...
// ...
// ...
// ...
var q1;
var q2;
var q3;
q1 = q2;
// should error
q1 = q3;