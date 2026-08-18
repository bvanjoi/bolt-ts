// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceAndSelfReferentialConstraint.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function test(arg) {
  return arg;
}
var res1 = test({
  foo: true,
  bar() {}  
});
var res2 = test({
  foo: true,
  bar: function () {}  
});
var res3 = test({
  foo: true,
  bar: () => {}  
});