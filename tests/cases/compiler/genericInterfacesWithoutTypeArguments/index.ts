// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/genericInterfacesWithoutTypeArguments.ts`, Apache-2.0 License

interface I<T> { }
class C<T> { }
var i: I;
var c: C<I>;
