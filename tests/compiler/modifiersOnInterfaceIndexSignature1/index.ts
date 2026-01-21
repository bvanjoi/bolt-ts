// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/modifiersOnInterfaceIndexSignature1.ts`, Apache-2.0 License

interface I {
  public [a: string]: number;
  //~^ ERROR: 'public' modifier cannot appear on an index signature.
}

interface G {
  readonly [a: string]: number
}

class B {
  public [a: string]: number;
  //~^ ERROR: 'public' modifier cannot appear on an index signature.
}

class C {
  static [a: string]: number;
}

class D {
  readonly [a: string]: number;
}