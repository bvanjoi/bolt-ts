type Foo<T, Y> = {
  foo<U, J>(): Foo<U, J>;
};
type SubFoo<R, S> = Foo<S, R>;
declare function foo(): SubFoo<number, string>;
