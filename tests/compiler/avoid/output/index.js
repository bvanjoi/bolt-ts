// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/avoid.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f() {
  var x = 1;
}
var y = f();
var why = f();
var w;
w = f();
class C {
  g() {}
}
var z = new C().g();
var N = new f();