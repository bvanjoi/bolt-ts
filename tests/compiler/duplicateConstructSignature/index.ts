// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateConstructSignature.ts`, Apache-2.0 License

interface I {
    (): number;
    (): string;
}