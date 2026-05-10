// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/noSubtypeReduction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

export interface IA {
    arr: { A: number; }[];
}

export interface IAB {
    arr: { A: number; B: number; }[];
}

export function F(x: IA | IAB) {
    const useB = (t: number) => { };
    for (const el of x.arr) {
        if ('A' in el) { }
        if ('B' in el) {
            useB(el.B);
        }
    }
}