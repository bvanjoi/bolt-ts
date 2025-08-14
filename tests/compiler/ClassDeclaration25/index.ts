// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ClassDeclaration25.ts`, Apache-2.0 License

interface IList<T> {
  data(): T;
  next(): string;
}
class List<U> implements IList<U> {
  data(): U;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
  next(): string;
  //~^ ERROR: Function implementation is missing or not immediately following the declaration.
}
