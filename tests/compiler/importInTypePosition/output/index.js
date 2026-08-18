// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importInTypePosition.ts`, Apache-2.0 License
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