// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restTypeRetainsMappyness.ts`, Apache-2.0 License
function test(fn) {
  var arr = {};
  fn(...arr);
}