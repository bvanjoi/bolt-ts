// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inlineConditionalHasSimilarAssignability.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(a) {
  var b = 0;
  a = b;// ok
  
  var c = 0;
  a = c;
  var d = 0;
  a = d;// ok
  
  var e = 0;
  a = e;
}