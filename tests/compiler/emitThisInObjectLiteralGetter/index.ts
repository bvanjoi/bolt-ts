// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emitThisInObjectLiteralGetter.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: strict=false

const example = {
    get foo() {
        return item => this.bar(item);
    }
};
