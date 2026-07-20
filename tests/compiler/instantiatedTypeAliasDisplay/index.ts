// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instantiatedTypeAliasDisplay.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@run-fail

interface X<A> {
    a: A;
}
interface Y<B> {
    b: B;
}
type Z<A, B> = X<A> | Y<B>;

declare function f1<A>(): Z<A, number>;
declare function f2<A, B, C, D, E>(a: A, b: B, c: C, d: D): Z<A, string[]>;

const x1 = f1<string>();  // Z<string, number>
const x2 = f2({}, {}, {}, {});  // Z<{}, string[]>
