// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indirectGlobalSymbolPartOfObjectType.ts`, Apache-2.0 License

//@compiler-options: target=es6

export { }
const Symbol = globalThis.Symbol;
[][Symbol.iterator];