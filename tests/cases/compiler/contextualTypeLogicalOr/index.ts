// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTypeLogicalOr.ts`, Apache-2.0 License

let x = 123;
var a =
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4] ||
    x && [1, 2, 3, 4];
