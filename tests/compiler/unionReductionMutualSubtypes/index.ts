// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionReductionMutualSubtypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface ReturnVal {
    something(): void;
}

const k: ReturnVal = { something() { } }

declare const val: ReturnVal;
function run(options: { something?(b?: string): void }) {
    const something = options.something ?? val.something;
    something('');
}
