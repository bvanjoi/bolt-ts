// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyAccess3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var foo: boolean;
foo.toBAZ();
//~^ ERROR: Property 'toBAZ' does not exist on type 'boolean'. 
