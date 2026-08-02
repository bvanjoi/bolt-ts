declare namespace foo {
  interface IFoo<T> {}
}
declare namespace bar {
  export class Foo <T> implements foo.IFoo<T> {}
}
