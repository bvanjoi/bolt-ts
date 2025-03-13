// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/blockScopedBindingCaptureThisInFunction.ts`, Apache-2.0 License

() => function () {
  for (let someKey in {}) {
      this.helloWorld(); // type('this') -> any
      () => someKey;
  }
};