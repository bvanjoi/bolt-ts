// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/namespacesDeclaration2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

namespace N {
    function S() {}
}
namespace M {
    function F() {}
}

declare namespace ns {
    let f: number;
}

var foge: N.S;
//~^ ERROR: Namespace 'N' has no exported member 'S'.
var foo: M.F;
//~^ ERROR: Namespace 'M' has no exported member 'F'.
let x: ns.A;
//~^ ERROR: Namespace 'ns' has no exported member 'A'.