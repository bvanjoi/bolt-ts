declare namespace Foo {
  export class B<T> {}
  export class A {}
}
declare var a: Foo.B<Foo.A>;

