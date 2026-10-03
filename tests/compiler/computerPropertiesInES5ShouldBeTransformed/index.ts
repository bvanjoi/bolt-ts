// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/computerPropertiesInES5ShouldBeTransformed.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

const b = ({ [`key`]: renamed }) => renamed;