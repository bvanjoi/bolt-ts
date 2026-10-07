// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constraintReferencingTypeParameterFromSameTypeParameterList.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function f() {}// Error, any does not satisfy the constraint I1<T, any>
// No error

function foo() {}