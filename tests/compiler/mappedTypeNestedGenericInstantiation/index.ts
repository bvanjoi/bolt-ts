// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/mappedTypeNestedGenericInstantiation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface Chainable<T> {
  value(): T;
  mapValues<U>(func: (v: T[keyof T]) => U): Chainable<{[k in keyof T]: U}>;
}

declare function chain<T>(t: T): Chainable<T>;

const square = (x: number) => x * x;

const v = chain({a: 1, b: 2}).mapValues(square).value();
