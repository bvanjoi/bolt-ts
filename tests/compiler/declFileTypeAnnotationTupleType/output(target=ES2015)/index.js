// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationTupleType.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=ES5
//@[target=ES2015]  compiler-options: target=ES2015
//@compiler-options: declaration
class c {}
var m = {};
(function (m) {

  class c {}
  m.c = c;
  
  class g {}
  m// Just the name
  .g = g;
  
})(m);
class g {}
var k = [new c(), new m.c()];
var l = k;
var x = [new g(), new m.g(), () => (new c())];
var y = x;