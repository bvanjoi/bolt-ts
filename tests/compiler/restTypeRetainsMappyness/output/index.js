// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restTypeRetainsMappyness.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function test(fn) {
  var arr = {};
  fn(...arr)// Error: Argument of type 'any[]' is not assignable to parameter of type 'Foo<T>'
  ;
}