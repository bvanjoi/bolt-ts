var A = {};
(function (A) {

  class Point {
    constructor(x, y) {
      this.x = x
      
      this.y = y}
  }
  A.Point = Point;
  
  var Origin = new Point(0, 0);
  A.Origin = Origin
  
})(A);
var B = {};
(function (B) {

  var a = A
  
})(B);
var C = {};
(function (C) {

  var a = A
  
  var m;
  
  var p;
  
  var p = {
      x: 0,
    y: 0    
  };
  
})(C);