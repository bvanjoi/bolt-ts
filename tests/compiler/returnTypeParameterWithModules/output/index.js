// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/returnTypeParameterWithModules.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var M1 = {};
(function (M1) {

  function reduce(ar, f, e) {
    return Array.prototype.reduce.apply(ar, e ? [f, e] : [f]);
  }
  M1.reduce = reduce;
  
  ;
  
})(M1);
;
var M2 = {};
(function (M2) {

  var A = M1
  
  function compose() {
    A.reduce(arguments, compose2);
  }
  M2.compose = compose;
  
  ;
  
  function compose2(g, f) {
    return function (x) {
      return g(f(x));
    };
  }
  M2.compose2 = compose2;
  
  ;
  
})(M2);
;