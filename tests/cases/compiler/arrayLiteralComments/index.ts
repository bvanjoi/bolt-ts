// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/arrayLiteralComments.ts`, Apache-2.0 License

var testArrayWithFunc = [
    // Function comment
    function() {
        let x = 1;
    },
    // String comment
    '1',
    // Numeric comment
    2,
    // Object comment
    { a: 1 },
    // Array comment
    [1, 2, 3]
]