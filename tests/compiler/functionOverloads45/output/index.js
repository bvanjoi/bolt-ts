// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads45.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo1([x]) {
  return undefined;
}
function foo2([x]) {
  return undefined;
}
var x1 = foo1([{
  a: 'str'  
}]);
var y1 = foo1([{
  a: 100  
}]);
var x2 = foo2([{
  a: 'str'  
}]);
var y2 = foo2([{
  a: 100  
}]);