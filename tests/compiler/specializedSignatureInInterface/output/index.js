// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/specializedSignatureInInterface.ts`, Apache-2.0 License
function f(a, b) {
  var a0 = a('foo');
  var b0 = b('foo');
  var b1 = b('bar');
}