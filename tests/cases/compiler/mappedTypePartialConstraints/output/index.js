
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/mappedTypePartialConstraints.ts`, Apache-2.0 License
class MyClass {
  doIt(data) {}
}
class MySubClass extends MyClass {}
function fn(arg) {}

fn(MySubClass);