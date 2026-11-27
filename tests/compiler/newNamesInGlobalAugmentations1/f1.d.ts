export {};

declare namespace M.M1 {
    export let x: number;
}
declare global {
    interface SymbolConstructor {
        observable: symbol;
    }
    class Cls {x}
    let [a, b]: number[];
    export import X = M.M1.x;
}