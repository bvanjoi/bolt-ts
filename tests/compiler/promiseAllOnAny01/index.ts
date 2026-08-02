// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseAllOnAny01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noEmit
//@compiler-options: lib=[es5, es2015.promise]

async function foo(x: any) {
    let abc = await Promise.all(x);
    let result: any[] = abc;
    return result;
}
