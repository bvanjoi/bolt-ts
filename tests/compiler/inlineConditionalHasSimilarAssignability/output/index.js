// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inlineConditionalHasSimilarAssignability.ts`, Apache-2.0 License
function foo(a) {
  var b = 0;
  a = b;
  var c = 0;
  a = c;
  var d = 0;
  a = d;
  var e = 0;
  a = e;
}