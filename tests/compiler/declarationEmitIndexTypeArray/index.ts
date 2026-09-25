// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitIndexTypeArray.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function doSomethingWithKeys<T>(...keys: (keyof T)[]) { }

const utilityFunctions = {
  doSomethingWithKeys
};
