// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterLeak.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict

var b = f({
  x: '',
  y: ''  
}).getBox();
if (b) {
  var x = b.data;
}
