// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noObjectKeysToKeyofT.ts`, Apache-2.0 License
//@compiler-options: target=es2016
// Do not change Object.keys to return keyof T.
Object.keys({
  a: 0  
}).push('b');