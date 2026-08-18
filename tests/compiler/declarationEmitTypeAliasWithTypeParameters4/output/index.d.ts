type Foo<T, Y> = {
  foo<U, J>(): Foo<U, J>;
};
type SubFoo<R> = Foo<string, R>;
declare function foo(): SubFoo<number>;
