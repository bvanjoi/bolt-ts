// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/moduleReopenedTypeOtherBlock.ts`, Apache-2.0 License

module M {
    export class C1 { }
    export interface I { n: number; }
}
module M {
    export class C2 { f(): I { return null; } }
}
