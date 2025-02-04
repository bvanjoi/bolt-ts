// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeParametersInStaticProperties.ts`, Apache-2.0 License

class foo<T> {
  static P: T;
  //~^ ERROR: Static members cannot reference class type parameters.
} 