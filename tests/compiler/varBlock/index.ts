// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/varBlock.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace m2 {

    export var a, b2: number = 10, b;
}

declare namespace m3 {
    var a, b, c;
    var a1, b1 = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.

    class C {
        constructor (public c = 10);
        //~^ ERROR: A parameter property is only allowed in a constructor implementation.
        //~| ERROR: A parameter initializer is only allowed in a function or constructor implementation.
    }
}

declare var b = 10;
//~^ ERROR: Initializers are not allowed in ambient contexts.

declare var a2, b2, c2;



declare var da = 10;
//~^ ERROR: Initializers are not allowed in ambient contexts.
declare var d3, d4 = 10;
//~^ ERROR: Initializers are not allowed in ambient contexts.

namespace m3 {
    declare var d = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    declare var d2, d3 = 10, d4 = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    //~| ERROR: Initializers are not allowed in ambient contexts.
    export declare var dE = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    export declare var d2E, d3E = 10, d4E = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    //~| ERROR: Initializers are not allowed in ambient contexts.
}

declare namespace m4 {
    var d = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    var d2, d3 = 10, d4 =10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    //~| ERROR: Initializers are not allowed in ambient contexts.
    export var dE = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    export var d2E, d3E = 10, d4E = 10;
    //~^ ERROR: Initializers are not allowed in ambient contexts.
    //~| ERROR: Initializers are not allowed in ambient contexts.
}

declare var c;
declare var c = 10;
//~^ ERROR: Initializers are not allowed in ambient contexts.
//~| ERROR: Subsequent variable declarations must have the same type. Variable 'c' must be of type 'any', but here has type 'number'.
