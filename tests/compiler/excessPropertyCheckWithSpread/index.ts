// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/excessPropertyCheckWithSpread.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare function f({ a: number }): void
//~^ ERROR: 'number' is an unused renaming of 'a'. Did you intend to use it as a type annotation?
interface I {
    readonly n: number;
}
declare let i: I;
f({ a: 1, ...i });

interface R {
    opt?: number
}
interface L {
    opt: string
}
declare let l: L;
declare let r: R;
f({ a: 1, ...l, ...r });
