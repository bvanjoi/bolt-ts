// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullableTypes1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
function f1(x) {
  var y = x || 'hello';// NonNullable<T> | string
  
}
function error() {
  throw new Error()
}
function f2(x) {// NonNullable<T>

  return x || error();
}
function f3(x) {
  var y = x;// {}
  
}
function f4(obj) {
  if (obj.x === 'hello') {
    obj;// NonNullable<T>
    
  }
  
  if (obj.x) {
    obj;// NonNullable<T>
    
  }
  
  if (typeof obj.x === 'string') {
    obj;// NonNullable<T>
    
  }
  
}
class A {
  x = 'hello';
  foo() {
    var zz = this.x;// string
    
  }
}