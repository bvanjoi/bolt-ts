// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/incorrectRecursiveMappedTypeConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function sum<T extends { [P in T]: number }, K extends keyof T>(n: number, v: T, k: K) {
  //~^ ERROR: Type parameter 'T' has a circular constraint.
  //~| ERROR: Type parameter 'P' has a circular constraint.
    n += v[k];
    //~^ ERROR: Operator '+=' cannot be applied to types 'number' and 'T[K]'.
}

