// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spellingSuggestionGlobal3.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015

export {}
declare global { var x: any }
global.x // should not suggest `global` (GH#42209)
//~^ ERROR: Cannot find name 'global'.
