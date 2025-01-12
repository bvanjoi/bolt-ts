// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/staticMethodsReferencingClassTypeParameters.ts`, Apache-2.0 License

class C<T> {
  static s(p: T) { return p; }
  //~^ ERROR: Static members cannot reference class type parameters.
}

class D {
  static s<T>(p: T) { return p; }
}