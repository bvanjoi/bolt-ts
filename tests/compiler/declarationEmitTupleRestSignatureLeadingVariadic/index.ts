
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTupleRestSignatureLeadingVariadic.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

const f = <TFirstArgs extends any[], TLastArg>(...args: [...TFirstArgs, TLastArg]): void => {};