var DEPS = {
  foo: 1  
};
test((deps, data) => (({
  fn1: function () {
    return deps.foo;
  },
  fn2: data.bar  
})), DEPS);
test((deps, data) => (({
  fn1: function () {
    return deps.foo;
  },
  fn2: data.bar  
})), DEPS);
test((deps, data) => (({
  fn1: () => (deps.foo),
  fn2: data.bar  
})), DEPS);
test((deps, data) => ({
  fn1() {
    return deps.foo;
  },
  fn2: data.bar  
}), DEPS);
test((deps) => (({
  fn1() {
    return deps.foo;
  },
  fn2: 1  
})), DEPS);