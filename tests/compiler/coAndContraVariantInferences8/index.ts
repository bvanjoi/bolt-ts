// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/coAndContraVariantInferences8.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/58468

declare const fn: (() => void) | ((a: number) => void);

declare const x: number;
declare const y: any;

fn.call(null, x);
fn.call(null, y);

export {};

function f0(a: {b: number | undefined}) {
  if (a.b) {
    const { b } = a;
    const c: string = b;
    //~^ ERROR: Type 'number' is not assignable to type 'string'
  }
}

function f1(a: {b: {c: number} | null}) {
  if (a.b) {
    const { b } = a;
    const c: string = b.c;
    //~^ ERROR: Type 'number' is not assignable to type 'string'
  }
}