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