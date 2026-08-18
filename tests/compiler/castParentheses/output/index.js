// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/castParentheses.ts`, Apache-2.0 License
class a {
  static b;
}
var b = (a);
var b = (a).b;
var b = (a.b).c;
var b = (a.b()).c;
var b = (new a());
var b = (new a.b());
var b = (new a()).b;