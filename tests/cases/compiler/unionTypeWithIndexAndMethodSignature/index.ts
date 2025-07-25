// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/unionTypeWithIndexAndMethodSignature.ts`, Apache-2.0 License

//@compiler-options: strict

interface Options {
  m(x: number): void;
  [key: string]: unknown;
}
declare function f(options: number | Options): void;
f({
  m(x) { },
});

f({
  m(x) {
    let a: string = x;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
   },
});

f(42);
f('42');
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number | Options'.
