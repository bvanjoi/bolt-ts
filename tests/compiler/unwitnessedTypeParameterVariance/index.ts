// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unwitnessedTypeParameterVariance.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

export interface CalcObj<O> {
    read: (origin: O) => CalcValue<O>;
}

export type CalcValue<O> = CalcObj<O>;

function foo<O>() {
    const unk: CalcObj<unknown> = { read: (origin: unknown) => unk }
    const x: CalcObj<O> = unk;
}

type A<T> = B<T>;

interface B<T> {
    prop: A<T>;
}

declare let a: A<number>;
declare let b: A<3>;
 
b = a;
