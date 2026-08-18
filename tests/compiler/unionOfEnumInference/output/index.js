// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionOfEnumInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Enum = {};
(function (Enum) {

  Enum[Enum['A'] = 0] = 'A'
  Enum[Enum['B'] = 0] = 'B'
  Enum[Enum['C'] = 0] = 'C'
})(Enum);
function foo(x) {}
function bar(x) {
  foo(x);
}