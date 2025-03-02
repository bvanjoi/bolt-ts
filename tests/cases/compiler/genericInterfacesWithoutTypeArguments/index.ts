// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericInterfacesWithoutTypeArguments.ts`, Apache-2.0 License

interface I<T> { }
class C<T> { }
var i: I;
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
var c: C<I>;
//~^ ERROR: Generic type 'I<T>' requires 1 type argument.
