// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleProperty2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    function f() {
        var x;
    }
    var y;
    export var z;
    var test1=x;  //~ ERROR: Cannot find name 'x'.
    var test2=y; // y visible because same module
}

namespace N {
    var test3=M.y; // nope y private property of M
    //~^ ERROR: Property 'y' does not exist on type 'typeof M'.
    var test4=M.z; // ok public property of M
}