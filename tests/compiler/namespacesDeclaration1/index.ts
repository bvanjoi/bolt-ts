// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/namespacesDeclaration1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace M {
   export namespace N {
      export namespace M2 {
         export interface I {}
      }
   }
}