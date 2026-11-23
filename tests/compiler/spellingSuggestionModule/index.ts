// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spellingSuggestionModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare module "foobar" { export const x: number; }
foobar;
//~^ ERROR: Cannot find name 'foobar'.

declare module 'barfoo' { export const x: number; }
barfoo;
//~^ ERROR: Cannot find name 'barfoo'.

declare namespace farboo { export const x: number; }
faroo;
//~^ ERROR: Cannot find name 'faroo'.
