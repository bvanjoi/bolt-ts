// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cyclicTypeInstantiation.ts`, Apache-2.0 License
function foo() {
  var x;
  return x
}
function bar() {
  var x;
  return x
}
var a = foo();
var b = bar();
// Relating types of a and b produces instantiations of the cyclic anonymous types in foo and bar
a = b;