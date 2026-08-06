// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/simpleRecursionWithBaseCase3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noImplicitAny
//@compiler-options: lib=[esnext]
//@compiler-options: noEmit

const fn1 = () => {
  if (Math.random() > 0.5) {
    return fn1()
  }
  return 0
}
