// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inferObjectTypeFromStringLiteralToKeyof.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false

var x = inference1(two);
var y = inference2({
  a: 1,
  b: 2,
  c: 3,
  d(n) {
    return n;
  }  
}, two);