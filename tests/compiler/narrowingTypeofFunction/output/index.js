// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingTypeofFunction.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f1(a) {
  if (typeof a === 'function') {
    a;
  } else {
    a;
  }
  
}
function f2(x) {
  if (typeof x === 'function') {
    x;
  } else {
    x;
  }
  
}
function f3(x) {
  if (typeof x === 'function') {
    x;
  }
  
}