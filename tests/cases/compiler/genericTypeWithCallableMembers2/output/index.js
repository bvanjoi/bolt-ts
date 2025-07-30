function foo1(f) {
  return f()
}
function foo2(f) {
  return new f()
}
var a = foo1(() => ('hello'));
;