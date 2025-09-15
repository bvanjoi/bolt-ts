// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/duplicateConstructorOverloadSignature2.ts`, Apache-2.0 License

class C<T> {
    constructor(x: T);
    constructor(x: T);
    constructor(x: any) { }
}