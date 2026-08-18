// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedToToIndexSignatureInference.ts`, Apache-2.0 License
//@compiler-options: target=esnext


fn(a);
var E = {};
(function (E) {

  E[E['A'] = 'foo'] = 'A'
  E[E['B'] = 'bar'] = 'B'
})(E);
var x = enumValues(E);