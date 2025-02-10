// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/substituteReturnTypeSatisfiesConstraint.ts`, Apache-2.0 License

type M = { p: string };
type O = { m: () => M };
type X<T extends M> = T;
type FFG<T> = T extends O ? X<ReturnType<T['m']>> : never;

// FFG<O> -> X<ReturnType<O['m']>> -> X<M> -> M -> {p: string}
// let a: FFG<O> = {p: 42};

let b: ReturnType<O['m']> = { p: 42 }
//~^ ERROR: Type 'number' is not assignable to type 'string'.

type Y<T> = T extends O ? T['m'] : never;
