// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndModuleInGlobal.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var _this = {};
(function (_this) {

  class c {}
  
})(_this);
var f = () => (this);