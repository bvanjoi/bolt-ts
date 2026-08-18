// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/bindingPatternOmittedExpressionNesting.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: declaration
//@run-fail

export let [,,[,[],,[],]] = undefined as any;