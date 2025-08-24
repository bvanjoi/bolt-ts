// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classExtendingQualifiedName2.ts`, Apache-2.0 License

module M {
    export class C {
    }

    class D extends M.C {
    }
}