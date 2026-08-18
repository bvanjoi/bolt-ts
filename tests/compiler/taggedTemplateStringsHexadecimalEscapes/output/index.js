// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedTemplateStringsHexadecimalEscapes.ts`, Apache-2.0 License
function f(...args) {}
f`x0D${'Interrupted CRLF'}x0A`;