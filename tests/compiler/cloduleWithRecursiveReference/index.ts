// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/cloduleWithRecursiveReference.ts`, Apache-2.0 License

module M
{
  export class C {  }
  export namespace C {
    export var C = M.C
  }
}