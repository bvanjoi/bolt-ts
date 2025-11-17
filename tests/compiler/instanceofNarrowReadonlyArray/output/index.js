function narrow(x) {
  if (x instanceof Array) {
    return x
  } else {
    return [x]
  }
  
}
var r1 = narrow([1, 2, 3]);
var r2 = narrow(42);