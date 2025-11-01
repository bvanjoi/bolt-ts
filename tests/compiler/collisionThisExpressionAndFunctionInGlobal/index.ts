// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/collisionThisExpressionAndFunctionInGlobal.ts`, Apache-2.0 License

function _this() { //Error
    return 10;
}
var f = () => this;
