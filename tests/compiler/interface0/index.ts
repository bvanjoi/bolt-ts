// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/interface0.ts`, Apache-2.0 License

interface Generic<T> {
    x: T;
}

var y: Generic<number> = { x: 3 };
