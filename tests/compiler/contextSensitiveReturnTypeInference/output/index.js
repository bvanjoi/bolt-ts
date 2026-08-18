// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextSensitiveReturnTypeInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
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