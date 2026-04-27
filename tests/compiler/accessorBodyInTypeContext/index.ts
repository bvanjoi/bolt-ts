// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/accessorBodyInTypeContext.ts`, Apache-2.0 License

type A = {
  get foo() { return 0 }
  //~^ ERROR: An implementation cannot be declared in type contexts.
};

type B = {
  set foo(v: any) { }
  //~^ ERROR: An implementation cannot be declared in type contexts.
};

interface X {
  get foo() { return 0 }
  //~^ ERROR: An implementation cannot be declared in type contexts.
}

interface Y {
  set foo(v: any) { }
  //~^ ERROR: An implementation cannot be declared in type contexts.
}

declare namespace C {
  class D {
    set foo(v: any) { } //~ERROR: An implementation cannot be declared in ambient contexts.
    get foo() { return 0 } //~ERROR: An implementation cannot be declared in ambient contexts.
  }
}

declare class E {
  set foo(v: any) { } //~ERROR: An implementation cannot be declared in ambient contexts.
  get foo() { return 0 } //~ERROR: An implementation cannot be declared in ambient contexts.
}