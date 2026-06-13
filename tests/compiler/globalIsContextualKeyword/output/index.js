function a() {
  var global = 1;
}
function b() {
  class global {}
}

function foo(global) {}
var obj = {
  global: '123'  
};