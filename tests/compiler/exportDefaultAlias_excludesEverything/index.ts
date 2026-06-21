// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultAlias_excludesEverything.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export default interface A {}
interface B {}
export default B;
//~^ ERROR: A module cannot have multiple default exports.
