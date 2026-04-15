var square = (x) => (x * x);
var v = chain({
  a: 1,
  b: 2  
}).mapValues(square).value();