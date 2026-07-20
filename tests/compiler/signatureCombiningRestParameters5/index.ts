// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/signatureCombiningRestParameters5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare const test1:
  | ((...args: [a: string | number, b: number | boolean]) => void)
  | ((...args: [c: number | boolean, d: string | boolean]) => void);

test1(42, true);
test1(42, [true]); // error
//~^ ERROR: Argument of type '(boolean)[]' is not assignable to parameter of type 'boolean'.

declare function test2<
  A extends readonly unknown[],
  B extends readonly unknown[],
>(
  c: (...args: A) => void,
  d: (...args: B) => void,
  e: (arg: typeof c | typeof d) => void,
): void;

test2(
  (a: number | boolean, b: string | number) => {},
  (c: string | boolean, d: number | boolean) => {},
  (cb) => {
    cb(true, 42);
    cb(true, [42]); // error
//~^ ERROR: Argument of type 'number[]' is not assignable to parameter of type 'number'.
  },
);
