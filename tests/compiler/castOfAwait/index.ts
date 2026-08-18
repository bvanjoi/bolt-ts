// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/castOfAwait.ts`, Apache-2.0 License

//@compiler-options: target=es6

async function f() {
    <number> await 0;
    typeof await 0;
    void await 0;
    await void <string> typeof <number> void await 0;
    //~^ ERROR: Conversion of type 'undefined' to type 'number' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
    await await 0;
}
