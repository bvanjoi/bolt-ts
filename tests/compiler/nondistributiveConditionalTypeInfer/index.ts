// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nondistributiveConditionalTypeInfer.ts`, Apache-2.0 License

type _R<T> = [T] extends [{ _R: (_: infer R) => void }] ? R : never;
type _E<T> = [T] extends [{ _E: () => infer E }] ? E : never;
type _A<T> = [T] extends [{ _A: () => infer A }] ? A : never;

interface Sync<R, E, A> {
  _R: (_: R) => void;
  _E: () => E;
  _A: () => A;
}

type R = _R<Sync<number, string, void>>;
type E = _E<Sync<number, string, void>>;
type A = _A<Sync<number, string, void>>;

let r: R = '42';
//~^ ERROR: Type 'string' is not assignable to type 'number'.
let e: E = 42;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
