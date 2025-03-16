// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericTypeReferencesRequireTypeArgs.ts`, Apache-2.0 License

class C<T> {
  foo(): T { return null }
}
interface I<T> {
  bar(): T;
}
var c1: C; // error
//~^ ERROR: Generic type 'C<T>' requires 1 type argument.
var i1: I; // error
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
var c2: C<I>; // should be an error
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
var i2: I<C>; // should be an error
//~^ ERROR: Generic type 'C<T>' requires 1 type argument.
