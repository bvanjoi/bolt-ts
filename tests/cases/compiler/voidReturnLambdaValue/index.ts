// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/voidReturnLambdaValue.ts`, Apache-2.0 License

function foo(arg1, arg2, callback:(v1,v2,v3) => void):void {
  return callback(arg1, arg2, arg2);
}