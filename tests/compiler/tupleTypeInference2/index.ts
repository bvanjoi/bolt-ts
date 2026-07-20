// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/tupleTypeInference2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

// Repro from #22564

type A<R> = [R] | [R, string];
declare function f<T>(x: A<T>): T;
f([undefined, ''] as [never, string]); // T: never
f([undefined, ''] as [void, string]); // T: void

// Repro from #22563

type B<R, S> = [R] | [R, S];
declare function g<T, U>(f: B<T, U>): U;
g([[]] as [void[]]); // U: {}

type C<R, S> = [R[]] | [R[], S];
declare function h<T, U>(f: C<T, U>): U;
h([[]] as [void[]]); // U: {}

// Repro from #22562

type C2<R> = [R[]] | [R[], void];
declare function h2<T>(f: C2<T>): T;
h2([[]] as [never[]]); // T: never
h2([[]] as [void[]]); // T: void
