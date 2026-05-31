// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorOverloads9.ts`, Apache-2.0 License

//@compiler-options: target=esnext
export class C {
    a;
    constructor();
    constructor(x = '') {
        this.a = x;
    }
}