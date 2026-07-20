// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleVisibilityTest4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
    export type nums = number;
}

namespace N {
    export type nums = number;
}

let a1: M.num;
//~^ ERROR: Namespace 'M' has no exported member 'num'.
let b1: M.nums;
let c1: M.bar;
//~^ ERROR: Namespace 'M' has no exported member 'bar'.

let a2: N.num;
//~^ ERROR: Namespace 'N' has no exported member 'num'.
let b2: N.nums;
let c2: N.bar;
//~^ ERROR: Namespace 'N' has no exported member 'bar'.
