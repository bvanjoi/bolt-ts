// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParametersInStaticAccessors.ts`, Apache-2.0 License

class foo<T> {
  static get Foo(): () => T { return null; }
  //~^ ERROR: Static members cannot reference class type parameters.
  static set Bar(v: { v: T }) { }
  //~^ ERROR: Static members cannot reference class type parameters.
} 