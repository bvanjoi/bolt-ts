// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveTypeParameterReferenceError2.ts`, Apache-2.0 License

interface List<T> {
  data: T;
  next: List<T>;
  owner: List<List<T>>;  // Error, recursive reference with wrapped T
}

interface List2<T> {
  data: T;
  next: List2<T>;
  owner: List2<List2<string>>;  // Ok
}
