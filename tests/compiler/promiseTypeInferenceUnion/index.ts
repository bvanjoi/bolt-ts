// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/promiseTypeInferenceUnion.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=esnext
//@compiler-options: lib=[esnext]


function f1(x: number): number | Promise<number> {
  return Promise.resolve(x);
}

function f2(x: number): number | PromiseLike<number> {
  return Promise.resolve(x);
}

function f3(x: number): number | Promise<number> | PromiseLike<number> {
  return Promise.resolve(x);
}

const g1: Promise<number> = Promise.resolve(f1(42));
const g2: Promise<number> = Promise.resolve(f2(42));
const g3: Promise<number> = Promise.resolve(f3(42));
