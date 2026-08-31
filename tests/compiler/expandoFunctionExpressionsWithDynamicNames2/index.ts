// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/expandoFunctionExpressionsWithDynamicNames2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: lib=[esnext]
//@compiler-options: noEmit

const mySymbol = Symbol();
interface Foo {
  (): void;
  [mySymbol]: true;
}
const foo: Foo = () => {};
foo[mySymbol] = true;

interface Bar {
  (): void;
  test: true;
}
const t = "test" as const;
const bar: Bar = () => {};
bar[t] = true;