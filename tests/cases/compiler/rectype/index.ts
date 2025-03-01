// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/rectype.ts`, Apache-2.0 License

module M {
  interface I { (i:I):I; }

  export function f(p: I) { return f };

  var i:I;

  f(i);
  f(f(i));
  f((f(f(i))));
}

