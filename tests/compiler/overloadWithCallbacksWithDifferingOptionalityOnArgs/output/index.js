// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadWithCallbacksWithDifferingOptionalityOnArgs.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function x2(callback) {}
x2(() => (1));
x2((x) => (1));