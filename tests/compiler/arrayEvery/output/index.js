var foo = ['aaa'];
var isString = (x) => (typeof x === 'string');
if (foo.every(isString)) {
  foo[0].slice(0);
}
