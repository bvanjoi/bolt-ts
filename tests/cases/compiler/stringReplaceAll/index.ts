// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/stringPropCodeGen.ts`, Apache-2.0 License

let a = "abcde".replaceAll("hello");
//~^ ERROR: Expected 2 arguments, but got 1.
let b = "abcde".replaceAll("hello", "world");
