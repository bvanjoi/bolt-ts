// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spellingSuggestionGlobal3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const global = { x: true }
globals.x // should suggest `global` (GH#42209)
//~^ ERROR: Cannot find name 'globals'.
