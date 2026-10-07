// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/collisionThisExpressionAndEnumInGlobal.ts`, Apache-2.0 License
var _this = // Error
{};
(function (_this) {

  _this[_this['_thisVal1'] = 0] = '_thisVal1'
  _this[_this['_thisVal2'] = 0] = '_thisVal2'
})(_this);
var f = () => (this);