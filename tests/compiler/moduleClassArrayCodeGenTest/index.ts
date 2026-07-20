// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleClassArrayCodeGenTest.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M
{
    export class A { }
    class B{ }
}

var t: M.A[] = [];
var t2: M.B[] = [];
//~^ ERROR: Namespace 'M' has no exported member 'B'.