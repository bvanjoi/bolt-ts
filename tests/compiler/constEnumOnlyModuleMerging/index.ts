// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constEnumOnlyModuleMerging.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Outer {
    export var x = 1;
}

namespace Outer {
    export const enum A { X }
}

namespace B {
    import O = Outer;
    var x = O.A.X;
    var y = O.x;
}
