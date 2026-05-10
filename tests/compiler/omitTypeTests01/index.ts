// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/omitTypeTests01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: module=commonjs

interface Foo {
    a: string;
    b: number;
    c: boolean;
}

export type Bar = Omit<Foo, "c">;
export type Baz = Omit<Foo, "b" | "c">;

export function getBarA(bar: Bar) {
    return bar.a;
}

export function getBazA(baz: Baz) {
    return baz.a;
}
