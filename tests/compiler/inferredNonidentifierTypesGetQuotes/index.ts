// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferredNonidentifierTypesGetQuotes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

var x = [{ "a-b": "string" }, {}];

var y = [{ ["a-b"]: "string" }, {}];