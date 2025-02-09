// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveCloduleReference.ts`, Apache-2.0 License

module M
{
  export class C {
  }
  export module C {
    export var C = M.C
  };
};
 
