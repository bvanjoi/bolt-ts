

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