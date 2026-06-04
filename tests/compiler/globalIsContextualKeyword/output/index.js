function a() {
  var global = 1;
}
function b() {
  class global {}
}

(function (global) {

})(global);
function foo(global) {}
var obj = {
  global: '123'  
};