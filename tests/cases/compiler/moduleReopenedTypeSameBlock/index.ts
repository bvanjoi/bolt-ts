// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/moduleCodeGenTest3.ts`, Apache-2.0 License

module M { export class C1 { } }
module M {
    export interface I { n: number; }
    export class C2 { f(): I { return null; } }
    export class C3 { f(): I { return { n: '42' } } }
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
}
