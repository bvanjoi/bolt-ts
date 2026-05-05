// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringsWithWhitespaceEscapesES6.ts`, Apache-2.0 License

//@compiler-options: target=es6

function f(...args: any[]) {
}

f `\t\n\v\f\r\\`;