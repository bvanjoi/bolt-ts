var Baz = {};
(function (Baz) {

  var x = 'hello';
  Baz.x = x
  
})(Baz);
Baz.x = 'goodbye';