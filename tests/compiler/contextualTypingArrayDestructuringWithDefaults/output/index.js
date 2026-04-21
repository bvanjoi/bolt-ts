var [c0 = {
  a: 'a'  
}] = [];
var [x1, c1 = {
  a: 'a'  
}] = [1];
var [c_ = {
  a: 'a'  
}] = [];
function foo() {
  var {length = {
      a: 1    
  }} = [1];
  return length
}