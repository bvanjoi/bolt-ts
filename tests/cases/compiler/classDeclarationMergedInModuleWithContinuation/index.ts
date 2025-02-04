// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/classDeclarationMergedInModuleWithContinuation.ts`, Apache-2.0 License

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