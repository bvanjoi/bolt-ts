// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericConstraintSatisfaction1.ts`, Apache-2.0 License

interface I<S> {
  f: <T extends S>(x: T) => void
}

var x: I<{s: string}>
x.f({s: 1})
//~^ ERROR: Type 'number' is not assignable to type 'string'.