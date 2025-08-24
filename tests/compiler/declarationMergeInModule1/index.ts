// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/declarationMergeInModule1.ts`, Apache-2.0 License

declare module A {
  interface B {}
}

declare module A {
  function b(b: B): void
}


{
  interface A<T = number> {
    a: T
  } 
  interface A<T> {
      b: T
  }
  function f(a: A) {
    let s0: string = a.a;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
    let s1: string = a.b;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
  }
}

{
  interface B<T> {
    b0: T;
  }
  interface B<T = number> {
    b1: T
  }
  function f(b: B) {
    let s0: string = b.b0;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
    let s1: string = b.b1;
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
  }
}