// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classExtendingQualifiedName.ts`, Apache-2.0 License

module M {
  class C {
  }

  class D extends M.C {
    //~^ ERROR: Property 'C' does not exist on type 'typeof M'.
  }
}