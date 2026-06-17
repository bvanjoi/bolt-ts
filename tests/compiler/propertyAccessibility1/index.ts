// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyAccessibility1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Foo {
  private privProp = 0;
}
var f = new Foo();
f.privProp;
//~^ ERROR: Property 'privProp' is private and only accessible within class 'Foo'.