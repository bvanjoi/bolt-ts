// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/addMoreOverloadsToBaseSignature.ts`, Apache-2.0 License

interface Foo {
  f(): string;
}

interface Bar extends Foo {
  f(key: string): string;
}
