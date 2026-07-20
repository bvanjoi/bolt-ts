// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyAccess1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var foo: { a: number; };
foo.a = 4;
foo.b = 5;
//~^ ERROR: Property 'b' does not exist on type '{ a: number; }'. 