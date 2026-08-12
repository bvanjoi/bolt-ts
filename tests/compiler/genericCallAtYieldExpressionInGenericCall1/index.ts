// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericCallAtYieldExpressionInGenericCall1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=es2015
//@compiler-options: lib=[esnext]
//@compiler-options: noEmit

declare const inner: {
  <A>(value: A): {
    (): A;
    [Symbol.iterator](): {
      next(...args: ReadonlyArray<any>): IteratorResult<any, A>;
    };
  };
};

declare function outer<A>(body: (value: A) => Generator<any, any, any>): void;

outer(function* <T>(value: T) {
  const result = yield* inner(value); // ok
});

outer(function* <T>(value: T) {
  const x = inner(value);
  const result = yield* x; // ok
});

declare const inner2: {
  <A>(value: A): () => A;
};

outer(function* <T>(value: T) {
  const result = yield* inner2(value); // error
  //~^ ERROR: Type '() => T' must have a '[Symbol.iterator]()' method that returns an iterator.
  //~| ERROR: Type '() => T' must have a '[Symbol.iterator]()' method that returns an iterator.
  //~| ERROR: Type '() => T' must have a '[Symbol.iterator]()' method that returns an iterator.
  //~| ERROR: Type '() => T' must have a '[Symbol.iterator]()' method that returns an iterator.
});

declare const inner3: {
  <A>(value: A): {
    (): A;
    [Symbol.iterator](): {
      next(...args: ReadonlyArray<any>): IteratorResult<number, A>;
    };
  };
};

declare function outer2<A, Y>(body: (value: A) => Generator<Y, any, any>): Y;

// number
const result1 = outer2(function* <T>(value: T) {
  yield* inner3(value);
});

// number
const result2 = outer2(function* <T>(value: T) {
  const x = inner3(value);
  yield* x;
});

declare function outer3<A>(
  body: (value: A) => Generator<never, unknown, unknown>,
): void;

// error
outer3(function* <T>(value: T) {
  //~^ ERROR: Argument of type '<T>(value: T) => Generator<number, void, never>' is not assignable to parameter of type '(value: unknown) => Generator<never, unknown, unknown>'.
  yield* inner3(value);
});

// error
outer3(function* <T>(value: T) {
  //~^ ERROR: Argument of type '<T>(value: T) => Generator<number, void, never>' is not assignable to parameter of type '(value: unknown) => Generator<never, unknown, unknown>'.
  const x = inner3(value);
  yield* x;
});
