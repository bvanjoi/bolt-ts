// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/classExtendingQualifiedName2.ts`, Apache-2.0 License

module M {
    export class C {
    }

    class D extends M.C {
    }
}