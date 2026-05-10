// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations-ambient.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: strict=false

declare const c1: boolean;
declare const c2: number;
declare const c3, c4 :string, c5: any;

declare namespace M {
    const c6;
    const c7: number;
}