// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorSupression1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo { static bar() { return "x"; } }

var baz = Foo.b;
//~^ ERROR: Property 'b' does not exist on type 'typeof Foo'.
  // Foo.b won't bind. 
baz.concat("y");

  // So we don't want an error on 'concat'.