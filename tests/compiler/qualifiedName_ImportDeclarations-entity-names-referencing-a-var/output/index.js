var Alpha = {};
(function (Alpha) {

  var x = 100;
  Alpha.x = x
  
})(Alpha);
var Beta = {};
(function (Beta) {

  var p = Alpha.x
  
})(Beta);
var x = Alpha.x;