// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndAliasInGlobal.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var a = {};
(function (a) {

  var b = 10;
  a.b = b
  
})(a);
var f = () => (this);
var _this = a