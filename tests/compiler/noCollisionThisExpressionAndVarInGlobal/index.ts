// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCollisionThisExpressionAndVarInGlobal.ts`, Apache-2.0 License

//@compiler-options: target=es2015
var _this = 1;
var f = () => _this;