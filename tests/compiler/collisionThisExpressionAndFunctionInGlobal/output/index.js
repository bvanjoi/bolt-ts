// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndFunctionInGlobal.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function _this() {
  return 10;
}
var f = () => (this);