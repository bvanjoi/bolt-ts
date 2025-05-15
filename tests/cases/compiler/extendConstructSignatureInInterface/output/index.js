
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/extendConstructSignatureInInterface.ts`, Apache-2.0 License
//@ run-fail
var CStatic;
class E extends CStatic {}
var e = new E(1);