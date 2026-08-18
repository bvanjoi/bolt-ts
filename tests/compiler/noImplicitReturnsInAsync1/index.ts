// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitReturnsInAsync1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitReturns

async function test(isError: boolean = false) {
    if (isError === true) {
        return;
    }
    let x = await Promise.resolve("The test is passed without an error.");
}