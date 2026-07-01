// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiModuleClodule1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    constructor(x: number) { }
    foo() { }
    bar() { }
    static boo() { }
}

namespace C {
    export var x = 1;
    var y = 2;
}
namespace C {
    export function foo() { }
    function baz() { return ''; }
}

var c = new C(C.x);
c.foo = C.foo;