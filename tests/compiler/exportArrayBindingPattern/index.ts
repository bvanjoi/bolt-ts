// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/exportArrayBindingPattern.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

const [a, , b] = [1, 2, 3];
export { a, b };