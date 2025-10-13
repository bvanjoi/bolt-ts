// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/stringTrim.ts`, Apache-2.0 License

//@compiler-options: target=es2019

var trimmed: string;
trimmed = "abcde".trimEnd();
trimmed = "abcde".trimStart();
trimmed = "abcde".trimLeft();
trimmed = "abcde".trimRight();
