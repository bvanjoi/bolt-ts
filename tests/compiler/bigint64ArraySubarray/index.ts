// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/bigint64ArraySubarray.ts`, Apache-2.0 License

//@compiler-options: target=es2020

function bigInt64ArraySubarray() {
    var arr = new BigInt64Array(10);
    arr.subarray();
    arr.subarray(0);
    arr.subarray(0, 10);
}
