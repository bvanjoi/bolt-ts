declare namespace foo {
  interface IFoo<T> {}
}
declare namespace bar {
  class Foo <T> implements foo.IFoo<T> {}
}
