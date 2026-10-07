// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallWithinOwnBodyCastTypeParameterIdentity.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var toThenable = (fn) => ((input) => {
  var result = fn(input);
  return {
      then(onFulfilled) {
      return toThenable(onFulfilled)(result);
    }    
  };
});
var toThenableInferred = (fn) => ((input) => {
  var result = fn(input);
  return {
      then(onFulfilled) {
      return toThenableInferred(onFulfilled)(result);
    }    
  };
});
var i = {
  f1(f) {}  
};