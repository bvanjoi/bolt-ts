// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndParametersTypeAliases.ts`, Apache-2.0 License
//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters
//@run-fail
// used in a declaration
// exported
// used in extends clause
// used in another type alias declaration
var x;
x();// used as type argument

var y;
y[0]();