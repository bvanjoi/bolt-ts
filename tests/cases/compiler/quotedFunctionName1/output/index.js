// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/quotedFunctionName1.ts`, Apache-2.0 License
class Test1 {
  'prop1'() {}
}
new Test1().prop1();