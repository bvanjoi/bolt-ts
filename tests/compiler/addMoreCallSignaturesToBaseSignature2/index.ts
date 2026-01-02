// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/addMoreCallSignaturesToBaseSignature2.ts`, Apache-2.0 License

interface Foo {
  (bar:number): string;
}

interface Bar extends Foo {
  (key: string): string;
}

var a: Bar;
var kitty = a(1);
var foo = a('');
var bar = a(false);
//~^ ERROR: No overload matches this call.