// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/propertyAccess1.ts`, Apache-2.0 License

var foo: { a: number; };
foo.a = 4;
foo.b = 5;
//~^ ERROR: Property 'b' does not exist on type 'object'. 