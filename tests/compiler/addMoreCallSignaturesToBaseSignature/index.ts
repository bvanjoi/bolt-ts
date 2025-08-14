// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/addMoreCallSignaturesToBaseSignature.ts`, Apache-2.0 License

interface Foo {
  (): string;
}

interface Bar extends Foo {
  (key: string): string;
}

var a: Bar;
var kitty = a();
var bar = a(42);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
