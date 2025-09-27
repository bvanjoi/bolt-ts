// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/deleteReadonlyInStrictNullChecks.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks

interface Function { readonly name: string; }
class Foo {}
delete Foo.name;
//~^ ERROR: The operand of a 'delete' operator cannot be a read-only property.
