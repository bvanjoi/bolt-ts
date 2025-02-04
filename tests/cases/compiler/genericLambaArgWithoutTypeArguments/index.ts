// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericLambaArgWithoutTypeArguments.ts`, Apache-2.0 License

interface Foo<T> {
  x: T;
}
function foo(a) {
  return null;
}
foo((arg: Foo) => { return arg.x; });
//~^ ERROR: Generic type 'Foo<T>' requires 1 type argument.