// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/getterSetterNonAccessor.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function getFunc() {
  return 0;
}
function setFunc(v) {}
Object.defineProperty({}, '0', ({
  get: getFunc,
  set: setFunc,
  configurable: true  
}));