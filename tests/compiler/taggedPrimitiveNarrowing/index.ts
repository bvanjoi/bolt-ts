// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedPrimitiveNarrowing.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

type Hash = string & { __hash: true };

function getHashLength(hash: Hash): number {
    if (typeof hash !== "string") {
        throw new Error("This doesn't look like a hash");
    }
    return hash.length;
}

function getHashLength2<T extends { __tag__: unknown}>(hash: string & T): number {
    if (typeof hash !== "string") {
        throw new Error("This doesn't look like a hash");
    }
    return hash.length;
}