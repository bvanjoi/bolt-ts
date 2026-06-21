// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingTypeofFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=true

type Meta = { foo: string }
interface F { (): string }

function f1(a: (F & Meta) | string) {
    if (typeof a === "function") {
        a;
    }
    else {
        a;
    }
}

function f2<T>(x: (T & F) | T & string) {
    if (typeof x === "function") {
        x;
    }
    else {
        x;
    }
}

function f3(x: { _foo: number } & number) {
    if (typeof x === "function") {
        x;
    }
}