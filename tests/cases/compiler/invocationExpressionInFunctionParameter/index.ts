// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/invocationExpressionInFunctionParameter.ts`, Apache-2.0 License

function foo1(val: string) {
}
function foo3(x = foo1(123)) { //~ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
}