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
