function fn() {
  return fn.n
}
var fn = {};
(function (fn) {

  var n = 1;
  fn.n = n
  
})(fn);