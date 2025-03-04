// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionVariableInReturnTypeAnnotation.ts`, Apache-2.0 License

function bar(): typeof b { //~ ERROR: Cannot find name 'b'.
  var b = 1;
  return undefined;
}