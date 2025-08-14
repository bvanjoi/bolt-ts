// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/extendConstructSignatureInInterface.ts`, Apache-2.0 License

//@ run-fail

interface C {
  new(x: number): C;
}

var CStatic: C;
class E extends CStatic {
}

var e: E = new E(1);
