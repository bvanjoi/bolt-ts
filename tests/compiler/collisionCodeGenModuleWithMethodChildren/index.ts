// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithMethodChildren.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict=false

namespace M {
    export var x = 3;
    class c {
        fn(M, p = x) { }
    }
}

namespace M {
    class d {
        fn2() {
            var M;
            var p = x;
        }
    }
}

namespace M {
    class e {
        fn3() {
            function M() {
                var p = x;
            }
        }
    }
}

namespace M { // Shouldnt bn _M
    class f {
        M() {
        }
    }
}