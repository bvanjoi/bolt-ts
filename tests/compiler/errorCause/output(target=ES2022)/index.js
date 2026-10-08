
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorCause.ts`, Apache-2.0 License
//@[target=ES2021]  compiler-options: target=es2021
//@[target=ES2022]  compiler-options: target=es2022
//@[target=ES2022]  run-fail
//@[target=ESNext]  compiler-options: target=esnext
//@[target=ESNext]  run-fail
var err = new Error('foo', {
  cause: new Error('bar')  
});
err.cause;
var anotherErr //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
= new Error(//~[target=ES2021]^ ERROR: Property 'cause' does not exist on type 'Error'.
'foo', {
  cause: a  
});
anotherErr.cause;
new EvalError//~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
('foo', {
  //~[target=ES2021]^ ERROR: Property 'cause' does not exist on type 'Error'.
  cause: new Error('bar')  
});
new EvalError('foo', {
  //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
  cause: a  
});
new RangeError('foo', {
  //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
  cause: new Error('bar')  
});
new ReferenceError('foo', {
  //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
  cause: new Error('bar')  
});
new SyntaxError('foo', {
  //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
  cause: new Error('bar')  
});
new TypeError('foo', {
  //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
  cause: new Error('bar')  
});
new URIError('foo', {
  //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
  cause: new Error('bar')  
});
new AggregateError([], //~[target=ES2021]^ ERROR: Expected 0-1 arguments, but got 2.
'foo', {
  cause: new Error('bar')  
});