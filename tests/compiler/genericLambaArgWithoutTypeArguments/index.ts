// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericLambaArgWithoutTypeArguments.ts`, Apache-2.0 License

interface Foo<T> {
  x: T;
}
function foo(a) {
//~^ ERROR: Parameter 'a' implicitly has an 'any' type.
  return null;
}
foo((arg: Foo) => { return arg.x; });
//~^ ERROR: Generic type 'Foo<T>' requires 1 type argument.
