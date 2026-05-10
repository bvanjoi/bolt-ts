// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/avoidNarrowingUsingConstVariableFromBindingElementWithLiteralInitializer.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare const foo: ["a", string, number] | ["b", string, boolean];

export function test(arg: { index?: number }) {
  const { index = 0 } = arg;

  if (foo[index] === "a") {
    foo;
  }
}
