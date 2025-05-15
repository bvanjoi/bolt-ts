// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/letKeepNamesOfTopLevelItems.ts`, Apache-2.0 License
var x;
function foo() {
  var x;
}
var A = {};
(function (A) {

  var x;
  
})(A);