class GenericObject {
  give(value) {
    return value
  }
}
class GenericNumber {
  give(value) {
    return value
  }
}
class GenericNumberOrString {
  give(value) {
    return value
  }
}
function g(type) {
  return new type()
}
var g1 = g(GenericObject);
g1.give({});
var g2 = g(GenericNumber);
g2.give(1);
var g3 = g(GenericNumberOrString);
g3.give(1);
g3.give('1');
class C {}
var g4 = g(C);