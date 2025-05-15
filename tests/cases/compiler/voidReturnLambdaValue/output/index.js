// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/voidReturnLambdaValue.ts`, Apache-2.0 License
function foo(arg1, arg2, callback) {
  return callback(arg1, arg2, arg2)
}