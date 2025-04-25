// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classDeclarationMergedInModuleWithContinuation.ts`, Apache-2.0 License

module M {
  export class N { }
  export module N {
      export var v = 0;
  }
}

module M {
  export class O extends M.N {
  }
}

let a0: M.N = new M.N();
let a1: number = M.N.v;
let a2: M.O = new M.O();