// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitInferredUndefinedPropFromFunctionInArray.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@compiler-options: module=commonjs

// repro from https://github.com/microsoft/TypeScript/issues/53914

export let b = [{ foo: 0, m() {} }, { bar: 1 }];