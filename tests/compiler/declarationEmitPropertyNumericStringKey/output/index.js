// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitPropertyNumericStringKey.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
// https://github.com/microsoft/TypeScript/issues/55292
var STATUS = {
  ['404']: 'not found'  
};
var hundredStr = '100';
var obj = {
  [hundredStr]: 'foo'  
};
var hundredNum = 100;
var obj2 = {
  [hundredNum]: 'bar'  
};