// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/getAndSetNotIdenticalType.ts`, Apache-2.0 License
class C {
  get x() {
    return 1
  }
  set x(v) {}
}