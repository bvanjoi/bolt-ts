// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisBinding.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

module M {
  export interface I {
z;
  }

  export class C {
public x=0;
f(x:I) {
    x.e;  // e not found
    //~^ ERROR: Property 'e' does not exist on type 'M.I'.
    x.z;  // ok 
}
  constructor() {
({z:10,f:this.f}).f(<I>({}));
  }
  }
}

class C {
  f(x: number) {
  }
}

