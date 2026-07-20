// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/voidReturnLambdaValue.ts`, Apache-2.0 License

function foo(arg1, arg2, callback:(v1,v2,v3) => void):void {
//~^ ERROR: Parameter 'arg1' implicitly has an 'any' type.
//~| ERROR: Parameter 'arg2' implicitly has an 'any' type.
//~| ERROR: Parameter 'v1' implicitly has an 'any' type.
//~| ERROR: Parameter 'v2' implicitly has an 'any' type.
//~| ERROR: Parameter 'v3' implicitly has an 'any' type.
  return callback(arg1, arg2, arg2);
}
