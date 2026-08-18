// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndAmbientVarInGlobal.ts`, Apache-2.0 License
//@compiler-options: target=es2015

var f = () => (this);
_this = 10;