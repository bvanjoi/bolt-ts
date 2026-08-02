type Foo<T> = {
  foo<U>(): Foo<U>;
};
declare function bar(): Foo<number>;
