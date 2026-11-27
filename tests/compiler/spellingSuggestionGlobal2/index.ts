// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spellingSuggestionGlobal2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export {}
declare global { const x: any }
const globals = { x: true }
global.x // should suggest `globals` (GH#42209)
//~^ ERROR: Cannot find name 'global'.
