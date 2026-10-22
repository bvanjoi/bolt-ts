// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleTest2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace T1 {
    namespace m3d { export var y = 2; }
    declare class m3d { constructor(foo); foo(): void ; static bar(); }
    var r = new m3d(); // error
    //~^ ERROR: Expected 1 arguments, but got 0.
}

namespace T2 {
    declare class m3d { constructor(foo); foo(): void; static bar(); }
    namespace m3d { export var y = 2; }
    var r = new m3d(); // error
    //~^ ERROR: Expected 1 arguments, but got 0.
}

namespace T3 {
    namespace m3d { export var y = 2; }
    declare class m3d { foo(): void; static bar(); }
    var r = new m3d();
    r.foo();
    r.bar(); // error
    //~^ ERROR: Property 'bar' does not exist on type 'm3d'.
    r.y; // error
    //~^ ERROR: Property 'y' does not exist on type 'm3d'.
}

namespace T4 {
    declare class m3d { foo(): void; static bar(); }
    namespace m3d { export var y = 2; }
    var r = new m3d();
    r.foo();
    r.bar(); // error
    //~^ ERROR: Property 'bar' does not exist on type 'm3d'.
    r.y; // error
    //~^ ERROR: Property 'y' does not exist on type 'm3d'.
}

namespace m3d { export var y = 2; }
declare class m3d { constructor(foo); foo(): void; static bar(); }
var r = new m3d(); // error
//~^ ERROR: Expected 1 arguments, but got 0.

declare class m4d extends m3d { }
var r2 = new m4d(); // error
//~^ ERROR: Expected 1 arguments, but got 0.
