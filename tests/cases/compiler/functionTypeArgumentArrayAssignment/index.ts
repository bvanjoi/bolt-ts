// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionTypeArgumentArrayAssignment.ts`, Apache-2.0 License

module test {
  interface Array<T> {
      foo: T;
      length: number;
  }

  function map<U>() {
      var ys: U[] = [];
  }
}
