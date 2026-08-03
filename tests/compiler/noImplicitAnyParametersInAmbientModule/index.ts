// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyParametersInAmbientModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

declare namespace D_M {
    // No implicit-'any' errors.
    function dm_f1(): void;

    // No implicit-'any' errors.
    function dm_f2(x): void;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // No implicit-'any' errors.
    function dm_f3(x: any): void;

    // No implicit-'any' errors.
    function dm_f4(x, y, z): void;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // No implicit-'any' errors.
    function dm_f5(x, y: any, z): void;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // No implicit-'any' errors.
    function dm_f6(...r): void;
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // No implicit-'any' errors.
    function dm_f7(x, ...r): void;
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
    //~| ERROR: Parameter 'x' implicitly has an 'any' type.

    // No implicit-'any' errors.
    function dm_f8(x1, y1: number): any;
    //~^ ERROR: Parameter 'x1' implicitly has an 'any' type.
    function dm_f8(x2: string, y2): any;
    //~^ ERROR: Parameter 'y2' implicitly has an 'any' type.
    function dm_f8(x3, y3): any;
    //~^ ERROR: Parameter 'x3' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y3' implicitly has an 'any' type.

    // No implicit-'any' errors.
    var dm_f9: () => string;

    // No implicit-'any' errors.
    var dm_f10: (x) => string;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // No implicit-'any' errors.
    var dm_f11: (x, y, z) => string;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // No implicit-'any' errors.
    var dm_f12: (x, y: any, z) => string;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // No implicit-'any' errors.
    var dm_f13: (...r) => string;
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // No implicit-'any' errors.
    var dm_f14: (x, ...r) => string;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
}