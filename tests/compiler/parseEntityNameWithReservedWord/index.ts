// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseEntityNameWithReservedWord.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum Bool { false }
const x: Bool.false = Bool.false;
