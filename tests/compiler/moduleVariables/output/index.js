
var x = 1;
var M = {};
(function (M) {

  var x = 2;
  M.x = x
  
  console.log(x);
  
})(M);

(function (M) {

  console.log(x);
  
})(M);

(function (M) {

  var x = 3;
  
  console.log(x);
  
})(M);