// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inferringAnyFunctionType4.ts`, Apache-2.0 License

function f<T extends (p1: number) => number>(p: T): T {
  return p;
}

var v = f(x => x);
f(x => {
  let y: string = x;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  return x;
})