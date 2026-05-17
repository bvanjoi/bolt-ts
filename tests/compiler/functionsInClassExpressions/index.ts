// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionsInClassExpressions.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let Foo = class {
    constructor() {
        this.bar++;
    }
    bar = 0;
    inc = () => {
        this.bar++;
    }
    m() { return this.bar; }
}