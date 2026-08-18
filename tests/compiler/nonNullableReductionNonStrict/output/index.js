// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullableReductionNonStrict.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function test(f1, f2) {
  f1('hello');
  f2('hello');
}
function f1(x) {
  var z = x;
}
function f2(x) {
  var z = x;
}