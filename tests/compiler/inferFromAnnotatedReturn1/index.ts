// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferFromAnnotatedReturn1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function test<T>(cb: (arg: T) => T): T;

const res1 = test((arg): number => 1); // ok
const res2 = test((arg): number => 'foo'); // error
//~^ ERROR: Type 'string' is not assignable to type 'number'.

export declare function linkedSignal<S, D>(options: {
  source: () => S;
  computation: (source: NoInfer<D>) => D;
}): D;

const signal = linkedSignal({
  source: () => 3,
  computation: (s): number => 3,
});

class Foo<T, R> {
  constructor(readonly cb: (t: T, _: { x: number; other: NoInfer<R> }) => R) {}
}

const _1 = new Foo((name: string, { x }): { name: string; x: number } => ({
  name,
  x,
}));
