// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doesNotNarrowUnionOfConstructorsWithInstanceof.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  length;
  constructor() {this.length = 1;}
}
class B {
  length;
  constructor() {this.length = 2;}
}
function getTypedArray(flag) {
  return flag ? new A() : new B();
}
function getTypedArrayConstructor(flag) {
  return flag ? A : B;
}
var a = getTypedArray(true);// A | B

var b = getTypedArrayConstructor(false);// A constructor | B constructor

if (!(a instanceof b)) {
  console.log(a.length);// Used to be property 'length' does not exist on type 'never'.
  
}
