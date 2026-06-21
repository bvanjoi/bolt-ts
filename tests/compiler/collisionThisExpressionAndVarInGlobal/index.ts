// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndVarInGlobal.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var _this = 1;
var f = () => this;