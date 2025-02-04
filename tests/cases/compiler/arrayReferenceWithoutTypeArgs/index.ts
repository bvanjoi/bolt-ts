// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/arrayReferenceWithoutTypeArgs.ts`, Apache-2.0 License

class X {
  public f(a: Array) { }
  //~^ ERROR: Generic type 'Array<T>' requires 1 type argument.
}