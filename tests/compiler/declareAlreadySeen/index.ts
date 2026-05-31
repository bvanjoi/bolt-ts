// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declareAlreadySeen.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    declare declare var x;
    //~^ ERROR: declare modifier already seen.
    declare declare function f();
    //~^ ERROR: declare modifier already seen.

    declare declare namespace N { }  
    //~^ ERROR: declare modifier already seen.

    declare declare class C { }
    //~^ ERROR: declare modifier already seen.
}