// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/implementClausePrecedingExtends.ts`, Apache-2.0 License

class C { foo: number }
class D implements C extends C { }
//~^ ERROR: 'extends' clause must precede 'implements' clause.