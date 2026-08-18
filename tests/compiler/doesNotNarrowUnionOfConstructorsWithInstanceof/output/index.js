// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doesNotNarrowUnionOfConstructorsWithInstanceof.ts`, Apache-2.0 License
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
var a = getTypedArray(true);
var b = getTypedArrayConstructor(false);
if (!(a instanceof b)) {
  console.log(a.length);
}
