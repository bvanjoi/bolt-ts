// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/keyofObjectWithGlobalSymbolIncluded.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

const obj = {
    [Symbol.species]: Array
};

type Q = keyof typeof obj;

const q: Q = Symbol.species;
