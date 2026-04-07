// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminateObjectTypesOnly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Thing = number | object;
const k: Thing = { toFixed: null }; // OK, satisfies object

type Thing2 = number | { toFixed: null } | object;
const q: Thing2 = { toFixed: null };
const h: Thing2 = { toString: null }; // OK, satisfies object

type Thing3 = number | { toFixed: null, toString: undefined } | object;
const l: Thing3 = { toString: undefined }; // error, toFixed isn't null
