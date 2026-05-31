// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithEnumMemberConflict.ts`, Apache-2.0 License

//@compiler-options: target=es2015
namespace m1 {
    enum e {
        m1, 
        m2 = m1
    }
}