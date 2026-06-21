function A() {
  return ''
}
function B() {
  var someLocal = {};
  return someLocal
}
class C {
  A() {
    return ''
  }
  B() {
    var someLocal = {};
    return someLocal
  }
}