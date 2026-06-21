// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterPropertyInConstructor4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: strict

export class C {
    constructor(public a: number[] = [], b: number) {
    }
}