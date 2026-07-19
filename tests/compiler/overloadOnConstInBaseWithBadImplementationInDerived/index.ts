// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstInBaseWithBadImplementationInDerived.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
  x1(a: number, callback: (x: 'hi') => number);
}

class C implements I {
  x1(a: number, callback: (x: 'hi') => number) { // error
  }
}
