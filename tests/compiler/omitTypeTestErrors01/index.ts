// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/omitTypeTestErrors01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

interface Foo {
    a: string;
    b: number;
    c: boolean;
}

export type Bar = Omit<Foo, "c">;
export type Baz = Omit<Foo, "b" | "c">;

export function getBarC(bar: Bar) {
    return bar.c; //~ERROR: Property 'c' does not exist on type 'Bar'.
}

export function getBazB(baz: Baz) {
    return baz.b; //~ERROR: Property 'b' does not exist on type 'Baz'.
}

