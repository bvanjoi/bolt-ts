type Foo<T> = T | {
  x: Foo<T>;
};
declare var x: Foo<number[]>;

declare function returnSomeGlobalValue(): Foo<number[]>;
