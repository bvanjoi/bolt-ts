// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexedAccessImplicitlyAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

interface I { foof: number };
declare const i: I;
i.foo;
//~^ ERROR: Property 'foo' does not exist on type 'I'.
i["foo"];
//~^ ERROR: Element implicitly has an 'any' type because expression of type '"foo"' can't be used to index type 'I'.
i.foof;
i["foof"];
