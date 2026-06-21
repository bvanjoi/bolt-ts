// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/infiniteConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

// Both of the following types trigger the recursion limiter in getImmediateBaseConstraint

type T1<B extends { [K in keyof B]: Extract<B[Exclude<keyof B, K>], { val: string }>["val"] }> = B;
type T2<B extends { [K in keyof B]: B[Exclude<keyof B, K>]["val"] }> = B;
//~^ ERROR: Type '"val"' cannot be used to index type 'B[Exclude]'.

// Repros from #22950

type AProp<T extends { a: string }> = T

declare function myBug<
  T extends { [K in keyof T]: T[K] extends AProp<infer U> ? U : never }
>(arg: T): T

const out = myBug({obj1: {a: "test"}})

type Value<V extends string = string> = Record<"val", V>;
declare function value<V extends string>(val: V): Value<V>;

declare function ensureNoDuplicates<
  T extends {
    [K in keyof T]: Extract<T[K], Value>["val"] extends Extract<T[Exclude<keyof T, K>], Value>["val"]
      ? never
      : any
  }
>(vals: T): void;

const noError = ensureNoDuplicates({main: value("test"), alternate: value("test2")});

const shouldBeNoError = ensureNoDuplicates({main: value("test")});

const shouldBeError = ensureNoDuplicates({main: value("dup"), alternate: value("dup")});
//~^ ERROR: Type 'Value<"dup">' is not assignable to type 'never'.
//~| ERROR: Type 'Value<"dup">' is not assignable to type 'never'.

// Repro from #26448

type Cond<T> = T extends number ? number : never;
declare function function1<T extends {[K in keyof T]: Cond<T[K]>}>(): T[keyof T]["foo"];
//~^ ERROR: Type '"foo"' cannot be used to index type 'T[keyof T]'.

// Repro from #31823

export type Prepend<Elm, T extends unknown[]> =
  T extends unknown ?
  ((arg: Elm, ...rest: T) => void) extends ((...args: infer T2) => void) ? T2 :
  never :
  never;
export type ExactExtract<T, U> = T extends U ? U extends T ? T : never : never;

type Conv<T, U = T> =
  { 0: [T]; 1: Prepend<T, Conv<ExactExtract<U, T>>>;}[U extends T ? 0 : 1];
