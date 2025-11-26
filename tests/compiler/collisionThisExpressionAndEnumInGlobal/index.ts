// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/collisionThisExpressionAndEnumInGlobal.ts`, Apache-2.0 License

enum _this { // Error
    _thisVal1,
    _thisVal2,
}
var f = () => this;