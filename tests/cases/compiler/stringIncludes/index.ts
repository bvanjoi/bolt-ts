// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/stringIncludes.ts`, Apache-2.0 License

//@compiler-options: target=ES6

var includes: boolean;
includes = "abcde".includes("cd");
includes = "abcde".includes("cd", 2);