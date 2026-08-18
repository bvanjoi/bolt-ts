// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/doWhileUnreachableCode.ts`, Apache-2.0 License
function test() {
  var foo = 0;
  testLoop: do {
    foo++;
    continue testLoop;
  } while (function () {
    var x = 1;
    return false;
  }())
  return foo;
}