// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParametersInStaticProperties.ts`, Apache-2.0 License

class foo<T> {
  static P: T;
  //~^ ERROR: Static members cannot reference class type parameters.
} 