// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/argsInScope.ts`, Apache-2.0 License
class C {
  P(ii, j, k) {
    for ( var i = 0; i < arguments.length; i++) {}
  }
}
// WScript.Echo("param: " + arguments[i]);
var c = new C();
c.P(1, 2, 3);