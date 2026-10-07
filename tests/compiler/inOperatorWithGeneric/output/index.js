// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inOperatorWithGeneric.ts`, Apache-2.0 License
class C {
  foo(x) {
    for ( var p in x) {}
  }
}