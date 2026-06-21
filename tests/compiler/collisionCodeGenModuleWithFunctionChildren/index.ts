// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithFunctionChildren.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    export var x = 3;
    function fn(M, p = x) { }
}

namespace M {
    function fn2() {
        var M;
        var p = x;
    }
}

namespace M {
    function fn3() {
        function M() {
            var p = x;
        }
    }
}