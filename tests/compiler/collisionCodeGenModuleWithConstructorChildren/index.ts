// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenModuleWithConstructorChildren.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    export var x = 3;
    class c {
        constructor(M, p = x) {
        }
    }
}

namespace M {
    class d {
        constructor(private M, p = x) {
        }
    }
}

namespace M {
    class d2 {
        constructor() {
            var M = 10;
            var p = x;
        }
    }
}