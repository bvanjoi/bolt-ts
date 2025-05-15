// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/namespaces1.ts`, Apache-2.0 License
var X = {};
(function (X) {

  var Y = {};
  (function (Y) {
  
    
    
  })(Y);
  X.Y = Y;
  
  
  
})(X);
var x;
var x2;