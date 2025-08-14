var F = {};
(function (F) {

  class Helper {}
  
})(F);

(function (F) {

  class Helper {}
  
})(F);
var Foo = {};
(function (Foo) {

  class Helper {}
  
})(Foo);

(function (Foo) {

  class Helper {}
  
})(Foo);
var Gar = {};
(function (Gar) {

  var Foo = {};
  (function (Foo) {
  
    class Helper {}
    
  })(Foo);
  
  
  (function (Foo) {
  
    class Helper {}
    
  })(Foo);
  
})(Gar);