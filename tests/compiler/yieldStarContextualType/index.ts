// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/yieldStarContextualType.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: noEmit

declare const g: <T, U, V>() => Generator<T, U, V>;

function* f(): Generator<string, void, unknown> {
    const x1 = yield* g();
    const x2: number = yield* g();
}