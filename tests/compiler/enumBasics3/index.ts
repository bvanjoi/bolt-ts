// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumBasics3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
  export namespace N {
    export enum E1 {
      a = 1,
      b = a.a, // should error
      //~^ ERROR: Property 'a' does not exist on type 'E1.a'.
    }
  }
}

namespace M {
  export namespace N {
    export enum E2 {
      b = M.N.E1.a,
      c = M.N.E1.a.a, // should error
      //~^ ERROR: Property 'a' does not exist on type 'E1.a'.
    }
  }
}