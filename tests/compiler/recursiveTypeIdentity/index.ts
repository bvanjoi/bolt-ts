// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveTypeIdentity.ts`, Apache-2.0 License

interface A {
  <T extends A>(x: T): void;
}

interface B {
  <T extends B>(x: T): void;
}

interface C {
  (x: A): void;
  (x: B): void;
}