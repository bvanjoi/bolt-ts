// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParametersInStaticMethods.ts`, Apache-2.0 License

class foo<T> {
  static M(x: (x: T) => { x: { y: T } }) {
    //~^ ERROR: Static members cannot reference class type parameters.
    //~| ERROR: Static members cannot reference class type parameters.
  }
} 