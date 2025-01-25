// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/blockScopedBindingCaptureThisInFunction.ts`, Apache-2.0 License

//@ run-fail

() => function () {
  for (let someKey in {}) {
      this.helloWorld(); // type('this') -> any
      () => someKey;
  }
};