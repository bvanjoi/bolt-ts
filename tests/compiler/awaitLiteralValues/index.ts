// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/awaitLiteralValues.ts`, Apache-2.0 License

function awaitString() {
    await 'literal';
    //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

function awaitNumber() {
    await 1;
    //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

function awaitTrue() {
    await true;
    //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

function awaitFalse() {
    await false;
    //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

function awaitNull() {
    await null;
    //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}

function awaitUndefined() {
    await undefined;
    //~^ ERROR: 'await' expressions are only allowed within async functions and at the top levels of modules.
}
