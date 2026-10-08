// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/identicalGenericConditionalsWithInferRelated.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f(arg) {
  var x = null;
  var y = null;
  x = y;// is err, should be ok
  
  y = x;// is err, should be ok
  
}// repro from https://github.com/microsoft/TypeScript/issues/26627

class Y {
  decode(ctor) {
    throw new Error()
  }
}