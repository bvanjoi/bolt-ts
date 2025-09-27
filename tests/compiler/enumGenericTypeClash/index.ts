// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumGenericTypeClash.ts`, Apache-2.0 License

class X<A,B,C> { }
enum X { MyVal }
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.
