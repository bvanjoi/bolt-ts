// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferringAnyFunctionType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es2015]

function f<T extends { "0": (p1: number) => number }>(p: T): T {
    return p;
}

var v = f([x => x]);
