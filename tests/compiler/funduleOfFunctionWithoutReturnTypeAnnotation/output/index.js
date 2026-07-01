function fn() {
  return fn.n;
}

(function (fn) {

  var n = 1;
  fn.n = n
  
})(fn);