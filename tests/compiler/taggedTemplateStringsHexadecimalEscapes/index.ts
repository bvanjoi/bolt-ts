// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringsHexadecimalEscapes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f(...args: any[]) {
}

f `\x0D${ "Interrupted CRLF" }\x0A`;