// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypeForInitalizedVariablesFiltersUndefined.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const fInferred = ({ a = 0 } = {}) => a;
// const fInferred: ({ a }?: { a?: number; }) => number

const fAnnotated: typeof fInferred = ({ a = 0 } = {}) => a;

declare var t: { s: string } | undefined;
const { s } = t;
//~^ ERROR: Property '"s"' does not exist on type 'undefined | { s: string; }'.
function fst({ s } = t) { }
//~^ ERROR: Property '"s"' does not exist on type 'undefined | { s: string; }'.

