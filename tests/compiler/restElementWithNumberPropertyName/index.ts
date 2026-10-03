// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restElementWithNumberPropertyName.ts`, Apache-2.0 License

//@[target=es5]     compiler-options: target=es5
//@[target=es2015]  compiler-options: target=es2015

const { 0: a, ...b } = [0, 1, 2];