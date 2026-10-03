export function bar(func = () => (foo)) {
  var foo = 'in';
}
export function baz1(func = {
  f() {
    return foo;
  }  
}) {
  var foo = 'in';
}
export function baz2(func = function () {
  return foo;
}) {
  var foo = 'in';
}
export function baz3(func = class {
  x = foo;
}) {
  var foo = 'in';
}