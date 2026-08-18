// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inferParameterWithMethodCallInitializer.ts`, Apache-2.0 License
function getNumber() {
  return 1;
}
class Example {
  getNumber() {
    return 1;
  }
  doSomething(a = this.getNumber()) {
    return a;
  }
}
function weird(a = this.getNumber()) {
  return a;
}
class Weird {
  doSomething(a = this.getNumber()) {
    return a;
  }
}