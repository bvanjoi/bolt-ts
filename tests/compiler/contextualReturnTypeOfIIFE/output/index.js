// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualReturnTypeOfIIFE.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var test1 = (async () => ([1, 'two']))();
var test2 = new Promise((resolve) => (resolve([1, 'two'])));
var obj = {
  foo: (() => ([1, 'two']))()  
};