// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonNullFullInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

function testNonNullInference(numbers: number[]) {
    let last;

    for (const n of numbers) {
        if (n % 2) {
            return n;
        }

        last = n;
    }

    last;
    last!;
}

function testNonNullInferenceWithArrays(numbers: number[]) {
    let result;
    const arr = [];

    for (const n of numbers) {
        if (n % 2) {
            return [n];
        }

        arr.push(n);
        result = arr;
    }

    result;
    result!;
}