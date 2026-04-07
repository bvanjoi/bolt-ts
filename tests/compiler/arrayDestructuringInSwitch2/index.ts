// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayDestructuringInSwitch1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type X = { kind: "a", a: [1] } | { kind: "b", a: [] }

function foo(x: X): 1 {
  const { kind, a } = x;
  switch (kind) {
    case "a":
      return a[0];
    case "b":
      return 1;
    default:
      const [n] = a;
      //~^ ERROR: Type 'never' must have a '[Symbol.iterator]()' method that returns an iterator.
      return a;
  }
}
