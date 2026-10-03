
var err = new Error('foo', {
  cause: new Error('bar')  
});
err.cause;
var anotherErr = new Error('foo', {
  cause: a  
});
anotherErr.cause;
new EvalError('foo', {
  cause: new Error('bar')  
});
new EvalError('foo', {
  cause: a  
});
new RangeError('foo', {
  cause: new Error('bar')  
});
new ReferenceError('foo', {
  cause: new Error('bar')  
});
new SyntaxError('foo', {
  cause: new Error('bar')  
});
new TypeError('foo', {
  cause: new Error('bar')  
});
new URIError('foo', {
  cause: new Error('bar')  
});
new AggregateError([], 'foo', {
  cause: new Error('bar')  
});