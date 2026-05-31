// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidContinueInDownlevelAsync.ts`, Apache-2.0 License

//@compiler-options: target=es2015

async function func() {
    if (true) {
        continue;
        //~^ ERROR: A 'continue' statement can only be used within an enclosing iteration statement.
    }
    else {
        await 1;
    }
}
