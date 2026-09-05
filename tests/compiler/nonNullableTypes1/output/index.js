function f1(x) {
  var y = x || 'hello';
}
function error() {
  throw new Error()
}
function f2(x) {
  return x || error();
}
function f3(x) {
  var y = x;
}
function f4(obj) {
  if (obj.x === 'hello') {
    obj;
  }
  
  if (obj.x) {
    obj;
  }
  
  if (typeof obj.x === 'string') {
    obj;
  }
  
}
class A {
  x = 'hello';
  foo() {
    var zz = this.x;
  }
}