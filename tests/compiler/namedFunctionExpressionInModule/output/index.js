// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/namedFunctionExpressionInModule.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Variables = {};
(function (Variables) {

  var x = function bar(a, b, c) {};
  
  x(1, 2, 3);
  
})(Variables);