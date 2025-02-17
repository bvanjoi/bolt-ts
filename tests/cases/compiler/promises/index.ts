// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/promises.ts`, Apache-2.0 License

interface Promise<T> {
  then<U>(success?: (value: T) => U): Promise<U>;
  then<U>(success?: (value: T) => Promise<U>): Promise<U>;
  value: T;
}
