// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionWithNoBestCommonType1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

function foo() {
    return true;
    return bar();
}

function bar(): void {
}