// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferringAnyFunctionType5.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f<T extends { q: (p1: number) => number }>(p: T): T {
    return p;
}

var v = f({ q: x => x });