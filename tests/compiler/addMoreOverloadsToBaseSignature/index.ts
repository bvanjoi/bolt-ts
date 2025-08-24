// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/addMoreOverloadsToBaseSignature.ts`, Apache-2.0 License

interface Foo {
  f(): string;
}

interface Bar extends Foo {
  //~^ ERROR: Interface 'Bar' incorrectly extends interface 'Foo'.
  f(key: string): string;
}
