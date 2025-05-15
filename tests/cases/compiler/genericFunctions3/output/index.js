
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericFunctions3.ts`, Apache-2.0 License
//@ run-fail

// was Error: Overload signature is not compatible with function definition.
function from(arg) {
  return undefined
}
var ff = from(false).foo("");