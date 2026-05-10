// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unreachableFlowAfterFinally.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitReturns

function f() {
    let x = 100;
    try {
        throw "WAT"
    }
    catch (e) {

    }
    finally {
        return x;
    }
}