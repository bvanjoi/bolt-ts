// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeParameterHasSelfAsConstraint.ts`, Apache-2.0 License

function foo<T extends T>(x: T): number {
  //~^ ERROR: Type parameter 'T' has a circular constraint.
  return x;
  //~^ ERROR: Type 'T' is not assignable to type 'number'.
}

