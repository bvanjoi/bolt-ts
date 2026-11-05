// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reachabilityChecksNoCrash1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext
//@skip-message-match

export async function arrayFromAsync<T>(asyncIterable!: AsyncIterable<T>): Promise<T[]> {
    const out = [];
    for await (const v of asyncIterable) {
        out.push(await v);
    }
    return out;
}
