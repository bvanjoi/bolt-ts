// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/rectype.ts`, Apache-2.0 License

module M {
  interface I { (i:I):I; }

  export function f(p: I) { return f };

  var i:I;

  f(i);
  //~^ ERROR: Variable 'i' is used before being assigned.
  f(f(i));
  //~^ ERROR: Variable 'i' is used before being assigned.
  f((f(f(i))));
  //~^ ERROR: Variable 'i' is used before being assigned.
}

