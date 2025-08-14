// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateConstructSignature2.ts`, Apache-2.0 License

interface I<T> {
    (x: T): number;
    (x: T): string;
}