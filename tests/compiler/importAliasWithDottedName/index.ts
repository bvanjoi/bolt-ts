// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/importAliasWithDottedName.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
    export var x = 1;
    export namespace N {
        export var y = 2;
    }
}

namespace A {
    import N = M.N;
    var r = N.y;
    var r2 = M.N.y;
}