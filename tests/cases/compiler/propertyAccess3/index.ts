// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/propertyAccess3.ts`, Apache-2.0 License

var foo: boolean;
foo.toBAZ();
//~^ ERROR: Property 'toBAZ' does not exist on type 'boolean'. 
