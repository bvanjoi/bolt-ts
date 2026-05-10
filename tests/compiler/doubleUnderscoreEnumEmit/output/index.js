var Foo = {};
(function (Foo) {

  Foo[Foo['__a'] = 1] = '__a'
  Foo[Foo['(Anonymous function)'] = 2] = '(Anonymous function)'
  Foo[Foo['(Anonymous class)'] = 4] = '(Anonymous class)'
  Foo[Foo['__call'] = 10] = '__call'
})(Foo);

(function (Foo) {

  function ___call() {
    return 5
  }
  Foo.___call = ___call;
  
})(Foo);
function Bar() {
  return 'no'
}
var Bar = {};
(function (Bar) {

  function __call(x) {
    return 5
  }
  Bar.__call = __call;
  
})(Bar);