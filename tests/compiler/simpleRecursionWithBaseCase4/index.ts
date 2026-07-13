// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/simpleRecursionWithBaseCase4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: checkJs
//@compiler-options: noEmit

var fn2 = function(name) {
  fn2 = compose(this, 0, 1)
  return fn2(name)

  function compose(child, level, find) {
    if (child === find) {
      return level
    }
    return compose(child, level + 1, find)
  }
}

var d = fn2(1); // d: any
d.redefined();
