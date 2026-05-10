// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/awaitInClassInAsyncFunction.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: useDefineForClassFields=false

async function bar() {
    return 2;
}

async function foo() {
    return new class {
        baz = await bar();
        //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
    };
}

