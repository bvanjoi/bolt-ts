// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedTypeVariableInfersLiteral.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var foo = {};
foo.bar = function () {
  var self = this;
};
foo.zab = (function () {
  var self = this;
});