// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericConstraintSatisfaction1.ts`, Apache-2.0 License

interface I<S> {
  f: <T extends S>(x: T) => void
}

var x: I<{s: string}>
x.f({s: 1})
//~^ ERROR: Argument of type '{ s: number; }' is not assignable to parameter of type '{ s: string; }'.