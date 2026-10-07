// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fixingTypeParametersRepeatedly3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail
var derived;
var result = foo(derived, (d) => (d.toBase()));// bar should type check just like foo.
// result2 should have the same type as result

var result2 = bar(derived, (d) => (d.toBase()));