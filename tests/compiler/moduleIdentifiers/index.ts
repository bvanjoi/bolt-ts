// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleIdentifiers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M {
    interface P { x: number; y: number; }
    export var a = 1
}

//var p: M.P;
//var m: M = M;
var x1 = M.a;
//var x2 = m.a;
//var q: m.P;