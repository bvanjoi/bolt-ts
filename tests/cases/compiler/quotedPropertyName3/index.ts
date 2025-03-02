// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/quotedPropertyName3.ts`, Apache-2.0 License

class Test {
  "prop1": number;
  foo() {
      var x = () => this["prop1"];
      var y: number = x();
  }
}