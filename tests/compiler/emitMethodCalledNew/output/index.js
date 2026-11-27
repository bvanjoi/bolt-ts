// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emitMethodCalledNew.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
// https://github.com/microsoft/TypeScript/issues/55075
var a = {
  new(x) {
    return x + 1;
  }  
};
var b = {
  'new'(x) {
    return x + 1;
  }  
};
var c = {
  ['new'](x) {
    return x + 1;
  }  
};