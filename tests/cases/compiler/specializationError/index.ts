// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/specializationError.ts`, Apache-2.0 License

interface Promise<T> {
  then<U>(value: T): void;
}

interface Bar {
  bar(value: "Menu"): Promise<string>;
  bar<T>(value: string, element: string): Promise<T>;
  bar<T>(value: string): Promise<T>;
}
