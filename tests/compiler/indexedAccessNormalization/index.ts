// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexedAccessNormalization.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type MyMap<M extends object> = {
    [K in keyof M]: {
        x: number
    }
}

declare function g<T>(value?: T): void;

function f1<M extends object>(mymap: MyMap<M>, k: keyof M) {
    const elemofM = mymap[k];
    g(elemofM);
}

function f2<M extends object>(mymap: MyMap<M>, k: keyof M, z: { x: number }) {
    const q1: MyMap<M>[keyof M] = z;
    const q2: MyMap<M>[keyof M] | undefined = z;
    const q3: MyMap<M>[keyof M] | string = z;
}