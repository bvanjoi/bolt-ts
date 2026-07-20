// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedBindingCaptureThisInFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// https://github.com/Microsoft/TypeScript/issues/11038

() => function () {
  for (let someKey in {}) {
      this.helloWorld(); // type('this') -> any
      () => someKey;
  }
};