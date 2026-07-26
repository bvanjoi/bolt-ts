// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashInYieldStarInAsyncFunction.ts`, Apache-2.0 License

//@compiler-options: target=esnext

// https://github.com/microsoft/TypeScript/issues/53145
var obj = {
    [Symbol.asyncIterator]() {
        return {
            next() {
                return { then() { } };
            }
        };
    }
};

async function* g() {
    yield* obj;
    //~^ ERROR: Type of 'await' operand must either be a valid promise or must not contain a callable 'then' member.
}