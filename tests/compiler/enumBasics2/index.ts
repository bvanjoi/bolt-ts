// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/enumBasics2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

enum Foo {
  a = 2,
  b = 3,
  x = a.b,       // should error  //~ERROR: Property 'b' does not exist on type 'Foo.a'.
  y = b.a,       // should error  //~ERROR: Property 'a' does not exist on type 'Foo.b'.
  z = y.x * a.x, // should error  //~ERROR: Property 'x' does not exist on type 'Foo.y'.
                                  //~| ERROR: Property 'x' does not exist on type 'Foo.a'.
}

enum Bar {
  a = (1).valueOf(),   // ok
  b = Foo.a,           // ok
  c = Foo.a.valueOf(), // ok
  d = Foo.a.a,         // should error  //~ERROR: Property 'a' does not exist on type 'Foo.a'.
}
