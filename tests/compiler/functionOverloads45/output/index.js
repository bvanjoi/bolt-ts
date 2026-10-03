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