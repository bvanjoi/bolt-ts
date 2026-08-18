// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/coAndContraVariantInferences.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration


foo(a, fab);
foo(b, fab);
var actionA = {
  payload: 'any-string'  
};
var actionB = {
  payload: true  
};
function call(action, fn) {
  fn(action);
}
var printFn = (action) => (console.log(action));
call(actionA, printFn);
call(actionB, printFn);