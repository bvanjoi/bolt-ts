// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctionsAndConditionalInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
function foo(obj) {
  return unboxify(obj);
}
var qq = foo({
  u: {
      value: 10    
  },
  v: {
      value: 'hello'    
  }  
});// { u: U, v: V } but should be { u: number, v: string }
// From #42385

var left = {};
var right = {};
var ok = (at) => (({
  lr: at.lr(at.str, at.num)  
}));
var orphaned = (at) => (at.dict(ok(at)));
var leftOk = ok(left);
var leftOrphaned = orphaned(left);
var rightOk = ok(right);
var rightOrphaned = orphaned(right);