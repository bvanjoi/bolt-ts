// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/excessPropertyCheckWithEmptyObject.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace M {
    export export var x = 1;
    //~^ ERROR: export modifier already seen.
    export export function f() { }
    //~^ ERROR: export modifier already seen.

    export export namespace N {
        //~^ ERROR: export modifier already seen.
        export export class C { }
        //~^ ERROR: export modifier already seen.
        export export interface I { }
        //~^ ERROR: export modifier already seen.
    }  
}

declare namespace A {
    export export var x;
    //~^ ERROR: export modifier already seen.
    export export function f()
    //~^ ERROR: export modifier already seen.

    export export namespace N {
        //~^ ERROR: export modifier already seen.
        export export class C { }
        //~^ ERROR: export modifier already seen.
        export export interface I { }
        //~^ ERROR: export modifier already seen.
    }
}