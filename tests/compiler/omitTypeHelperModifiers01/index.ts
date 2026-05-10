// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/omitTypeHelperModifiers01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type A = {
    a: number;
    b?: string;
    readonly c: boolean;
    d: unknown;
};

type B = Omit<A, 'a'>;

function f(x: B) {
    const b = x.b;
    x.b = "hello";
    x.b = undefined;

    const c = x.c;
    x.c = true;     //~ERROR: Cannot assign to 'c' because it is a read-only property.

    const d = x.d;
    x.d = d;
}
