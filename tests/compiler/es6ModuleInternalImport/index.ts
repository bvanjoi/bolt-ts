// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ModuleInternalImport.ts`, Apache-2.0 License

//@compiler-options: target=es6

namespace n0 {
    export var a = 10;
}
namespace n1 {
    export import a3 = n0.a;
}
const n2: string = n1.a3;
//~^ ERROR: Type 'number' is not assignable to type 'string'.
const n3: string = n0.a;
//~^ ERROR: Type 'number' is not assignable to type 'string'.

export namespace m {
    export var a = 10;
}
export import a1 = m.a;
import a2 = m.a;
var x = a1 + a2;
export namespace m1 {
    export import a3 = m.a;
    import a4 = m.a;
    var x = a1 + a2;
    var x2 = a3 + a4;
}
namespace m2 {
    export import a3 = m.a;
    import a4 = m.a;
    var x = a1 + a2;
    var x2 = a3 + a4;
    var x4 = m1.a3 + m2.a3;
}