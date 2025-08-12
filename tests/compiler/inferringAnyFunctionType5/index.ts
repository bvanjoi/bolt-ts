// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inferringAnyFunctionType5.ts`, Apache-2.0 License

function f<T extends { q: (p1: number) => number }>(p: T): T {
  return p;
}

var v = f({ q: x => x });

var v2 = f({ q: x => x + 1 }).q(1);
var v3: number = f({ q: x => x + 1 }).q(1);
