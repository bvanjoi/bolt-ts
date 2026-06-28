class Foo {}
function maker(value) {
  return maker.Bar;
}

(function (maker) {

  class Bar extends Foo {}
  maker.Bar = Bar;
  
})(maker);
maker('42');