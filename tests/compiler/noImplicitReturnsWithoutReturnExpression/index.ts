// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/noImplicitReturnsWithoutReturnExpression.ts`, Apache-2.0 License

//@compiler-options: noImplicitReturns

function isMissingReturnExpression(): number {
    return;
    //~^ ERROR: Not all code paths return a value.
}

function isMissingReturnExpression2(): any {
    return;
}

function isMissingReturnExpression3(): number|void {
    return;
}

function isMissingReturnExpression4(): void {
    return;
}

function isMissingReturnExpression5(x) {
    if (x) {
        return 0;
    }
    else {
        return;
        //~^ ERROR: Not all code paths return a value.
    }
}
