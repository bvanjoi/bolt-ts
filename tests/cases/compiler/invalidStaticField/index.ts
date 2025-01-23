// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/invalidStaticField.ts`, Apache-2.0 License

class A { foo() { return B.NULL; } }
//~^ ERROR: Property 'NULL' does not exist on type 'typeof B'.
class B { static NOT_NULL = new B(); }