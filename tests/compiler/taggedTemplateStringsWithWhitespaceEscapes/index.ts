// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringsWithWhitespaceEscapes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f(...args: any[]) {
}

f `\t\n\v\f\r\\`;