// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/structural1.ts`, Apache-2.0 License

module M {
  export interface I {
      salt:number;
      pepper:number;
  }

  export function f(i:I) {
  }

  f({salt:2,pepper:0});
}
