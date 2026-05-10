// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/extendedUnicodeEscapeSequenceIdentifiers.ts`, Apache-2.0 License

//@compiler-options: target=es6

const \u{0061} = 12;
const a\u{0061} = 12;

console.log(a + aa);
