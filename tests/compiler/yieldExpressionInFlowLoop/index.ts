// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/yieldExpressionInFlowLoop.ts`, Apache-2.0 License

//@compiler-options: noImplicitAny
//@compiler-options: target=es2015

function* f() {
    let result;
    while (1) {
        result = yield result;
        //~^ ERROR: 'yield' expression implicitly results in an 'any' type because its containing generator lacks a return-type annotation.
    }
}
