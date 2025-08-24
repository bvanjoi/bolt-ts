// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/quotedFunctionName2.ts`, Apache-2.0 License

class Test1 {
  static "prop1"() { }
}

Test1.prop1();
