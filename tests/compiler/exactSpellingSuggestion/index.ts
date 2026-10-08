// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exactSpellingSuggestion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Fixes #16245 -- always suggest the exact match, even when
// other options are very close
enum U8 {
    BIT_0 = 1 << 0,
    BIT_1 = 1 << 1,
    BIT_2 = 1 << 2
}

U8.bit_2
//~^ ERROR: Property 'bit_2' does not exist on type '{ BIT_0: U8.BIT_0; BIT_1: U8.BIT_1; BIT_2: U8.BIT_2; }'
