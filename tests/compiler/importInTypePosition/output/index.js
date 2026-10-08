// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importInTypePosition.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var A = {};
(function (A) {

  class Point {
    constructor(x, y) {
      this.x = x
      
      this.y = y}
  }
  A.Point // no code gen expected
  = Point;
  
  var Origin = new Point//Error generates 'var <Alias> = <EntityName>;'
  (// no code gen expected
  0, 0);
  A.Origin =//Error generates 'var <Alias> = <EntityName>;'
   Origin
  
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