// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeofUsedBeforeBlockScoped.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type T = typeof C & typeof C.s & typeof o & typeof o.n;
class C {
    static s = 2;
}
type W = typeof o.n;
typeof o2;
//~^ ERROR: Block-scoped variable 'o2' used before its declaration.
let o2: typeof o;
let o = { n: 12 };
