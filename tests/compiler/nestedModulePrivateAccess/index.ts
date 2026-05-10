// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedModulePrivateAccess.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace a{
       var x:number;
       namespace b{
               var y = x; // should not be an error
       }
}