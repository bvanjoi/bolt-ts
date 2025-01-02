// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/implementsInClassExpression.ts`, Apache-2.0 License

interface Foo {
  doThing(): void;
}

let cls = class implements Foo {
  doThing() { }
}