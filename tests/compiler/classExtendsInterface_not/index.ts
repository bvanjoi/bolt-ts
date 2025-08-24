// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classExtendsInterface_not.ts`, Apache-2.0 License

class C extends "".bogus {}
//~^ ERROR: Property 'bogus' does not exist on type '""'.
