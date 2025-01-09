// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/innerTypeArgumentInference.ts`, Apache-2.0 License

interface Generator<T> { (): T; }
function Generate<U>(func: Generator<U>): U {
    return Generate(func);
}