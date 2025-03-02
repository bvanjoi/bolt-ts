// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/overloadOnConstInBaseWithBadImplementationInDerived.ts`, Apache-2.0 License

interface I {
  x1(a: number, callback: (x: 'hi') => number);
}

class C implements I {
  x1(a: number, callback: (x: 'hi') => number) { // error
  }
}