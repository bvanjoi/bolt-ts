function getNumber() {
  return 1
}
class Example {
  getNumber() {
    return 1
  }
  doSomething(a = this.getNumber()) {
    return a
  }
}
function weird(a = this.getNumber()) {
  return a
}
class Weird {
  doSomething(a = this.getNumber()) {
    return a
  }
}