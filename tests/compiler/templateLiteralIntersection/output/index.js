// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/templateLiteralIntersection.ts`, Apache-2.0 License
var a = 'a';
function f() {
  var b = {
      c: 1    
  };
  var d = a ? `\${${b.c++}}` : '';
  var type = 2;
  type = 1;
}