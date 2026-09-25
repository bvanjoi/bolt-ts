var CONFIG = {
  foo: '',
  setFoo: function (foo) {
    CONFIG.foo = foo;
  }  
};
var helper = function (t) {
  helper(t.slice(1));
};