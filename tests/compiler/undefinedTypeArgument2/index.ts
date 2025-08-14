// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/undefinedTypeArgument2.ts`, Apache-2.0 License

// once caused stack overflow
interface Query<T> {
  selectMany<U>(selector: (item: T) => U[]): Query<U>;
  selectMany<U>(arraySelector: (item: T) => U[], resultSelector: (outer: T, inner: U) => R): Query<R>;
  //~^ ERROR: Cannot find name 'R'.
  //~| ERROR: Cannot find name 'R'.
}