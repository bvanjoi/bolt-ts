// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/spreadTypeRemovesReadonly.ts`, Apache-2.0 License
var data = {
  value: 'foo'  
};
var clone = {
  ...data  
};
clone.value = 'bar';