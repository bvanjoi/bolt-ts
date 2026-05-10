// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleProperty1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
namespace M {
    var x=10;  // variable local to this module body
    var y=x;  // property visible only in module
    export var z=y;  // property visible to any code
}

namespace M2 {
    var x = 10;  // variable local to this module body
    private y = x;  // can't use private in modules
    //~^ ERROR: Declaration or statement expected.
    //~| ERROR: Cannot find name 'y'.
    export var z = y;  // property visible to any code
    //~^ ERROR: Cannot find name 'y'.
}