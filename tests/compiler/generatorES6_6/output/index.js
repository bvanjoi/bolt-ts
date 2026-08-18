// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/generatorES6_6.ts`, Apache-2.0 License
//@compiler-options: strict=false
class C {
  *[Symbol.iterator]() {
    var a = yield 1;
  }
}