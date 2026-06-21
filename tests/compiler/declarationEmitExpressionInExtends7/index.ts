// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitExpressionInExtends7.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

export default class extends SomeUndefinedFunction {}
//~^ ERROR: Cannot find name 'SomeUndefinedFunction'.