// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveTypes1.ts`, Apache-2.0 License

interface Entity<T extends Entity<T>> {
  X: T;
  Y: T;
}

interface Person<U extends Person<U>> extends Entity<U> {
 n: number;
}

interface Customer extends Person<Customer> {
 s: string;
}
