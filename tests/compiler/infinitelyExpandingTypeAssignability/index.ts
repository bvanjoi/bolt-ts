// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/infinitelyExpandingTypeAssignability.ts`, Apache-2.0 License

interface A<T> {
  x : T
}

interface B<T> extends A<B<B<B<T>>>> { }

interface C<T> extends A<C<C<C<T>>>> { }

var x : B<string>
var y : C<string> = x
