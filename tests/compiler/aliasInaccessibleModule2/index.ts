// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/aliasInaccessibleModule2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace M {
    namespace N {
        class C {
        }
        
    }
    import R = N;
    export import X = R;
}
