// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncFunctionTempVariableScoping.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: lib=[es2015]

// https://github.com/Microsoft/TypeScript/issues/19187

async ({ foo, bar, ...rest }) => bar(await foo);