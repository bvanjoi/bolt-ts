// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/duplicateConstructorOverloadSignature.ts`, Apache-2.0 License

class C {
    constructor(x: number);
    constructor(x: number);
    constructor(x: any) { }
}