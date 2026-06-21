// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeWithNameClauseAppliedToArrayType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Mappy<T extends unknown[]> = { [K in keyof T as K]: T[K] };
type NotArray = Mappy<number[]>;

declare function doArrayStuff(x: unknown[]): void;
declare const x: NotArray;
doArrayStuff(x);
