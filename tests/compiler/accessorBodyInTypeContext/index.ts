// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/accessorBodyInTypeContext.ts`, Apache-2.0 License

type A = {
  get foo() { return 0 }
  //~^ ERROR: An implementation cannot be declared in ambient contexts.
};

type B = {
  set foo(v: any) { }
  //~^ ERROR: An implementation cannot be declared in ambient contexts.
};

interface X {
  get foo() { return 0 }
  //~^ ERROR: An implementation cannot be declared in ambient contexts.
}

interface Y {
  set foo(v: any) { }
  //~^ ERROR: An implementation cannot be declared in ambient contexts.
}

