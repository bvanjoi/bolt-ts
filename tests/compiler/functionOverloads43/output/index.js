function foo([x]) {
  if (x) {
    return x.a
  }
  
  return undefined
}
var x = foo([{
  a: 'str'  
}]);
var y = foo([{
  a: 100  
}]);