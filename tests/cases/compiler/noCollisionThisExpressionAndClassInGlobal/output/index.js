// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/noCollisionThisExpressionAndClassInGlobal.ts`, Apache-2.0 License
class _this {}
var f = () => _this;