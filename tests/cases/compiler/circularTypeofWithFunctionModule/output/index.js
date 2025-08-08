class Foo {}
function maker(value) {
  return maker.Bar
}
var maker = {};
(function (maker) {

  class Bar extends Foo {}
  maker.Bar = Bar;
  
})(maker);