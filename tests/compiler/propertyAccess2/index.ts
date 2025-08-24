// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/propertyAccess2.ts`, Apache-2.0 License

var foo: number;
foo.toBAZ();
//~^ ERROR: Property 'toBAZ' does not exist on type 'number'. 