// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/asyncIIFE.ts`, Apache-2.0 License

//@compiler-options: target=ES6

function f1() {
    (async () => {
        await 10
        throw new Error();
    })();

    var x = 1;
}
