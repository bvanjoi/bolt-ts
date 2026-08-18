// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inKeywordAndIntersection.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
class A {
  a = 0;
}
class B {
  b = 0;
}
function f10(obj) {
  if (obj instanceof Object) {
    obj;
  } else {
    obj;
  }
  
}
var instance = {};
var ClassOne = {};
if (instance instanceof ClassOne) {
  instance.one();
}
