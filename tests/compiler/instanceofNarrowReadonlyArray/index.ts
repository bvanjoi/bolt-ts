// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/instanceofNarrowReadonlyArray.ts`, Apache-2.0 License

//@compiler-options: strict

function narrow(x: readonly number[] | number): readonly number[] {
    if (x instanceof Array) {
        return x;
    } else {
        return [x];
    }
}

const r1: readonly number[] = narrow([1, 2, 3]);
const r2: readonly number[] = narrow(42);