// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constraintPropagationThroughReturnTypes.ts`, Apache-2.0 License
function g(x) {
  return x
}
function f(x) {
  var y = g(x);
  y;
}
{
  var a = "";
  
  var test = (_) => {};
  test({key: "value"});
}
{
  var a = "";
  
  var test = (_) => {};
  test({key: "value"});
}