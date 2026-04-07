// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/unionSignaturesWithThisParameter.ts`, Apache-2.0 License

//@compiler-options: strict

function x<T>(ctor: {
  (this: {}, v: T): void;
  new(v: T): void;
} | {
  (v: T): void;
  new(v: T): void;
}, t: T) {
  new ctor(t);
}

function a(this: number): number { 
  return 42;
}
const ret: string = a.call(42);
//~^ ERROR: Type 'number' is not assignable to type 'string'.