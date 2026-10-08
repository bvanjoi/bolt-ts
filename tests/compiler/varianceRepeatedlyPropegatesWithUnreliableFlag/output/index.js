// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/voidAsOperator.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var i = null;
var p2 = null;
// Commenting out the below line will remove the error on the `const _i: I<A> = i;`
i.fn(null, p2);
var _i = i;