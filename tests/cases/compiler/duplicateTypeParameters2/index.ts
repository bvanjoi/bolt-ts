// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateTypeParameters2.ts`, Apache-2.0 License

class A { public foo() { } }
class B { public bar() { } }

interface I<T extends A, T extends B> {}
//~^ ERROR: Duplicate identifier 'T'.