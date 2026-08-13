// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyParametersInModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

namespace M {
    // No implicit-'any' errors.
    function m_f1(): void { }

    // Implicit-'any' error for x.
    function m_f2(x): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // No implicit-'any' errors.
    function m_f3(x: any): void { }

    // Implicit-'any' errors for x, y, and z.
    function m_f4(x, y, z): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x and z.
    function m_f5(x, y: any, z): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' error for r.
    function m_f6(...r): void { }
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x and r.
    function m_f7(x, ...r): void { }
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
    //~| ERROR: Parameter 'x' implicitly has an 'any' type.

    // Implicit-'any' errors for x1, y2, x3, and y3.
    function m_f8(x1, y1: number): any;
    //~^ ERROR: Parameter 'x1' implicitly has an 'any' type.
    function m_f8(x2: string, y2): any;
    //~^ ERROR: Parameter 'y2' implicitly has an 'any' type.
    function m_f8(x3, y3): any { }
    //~^ ERROR: Parameter 'x3' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y3' implicitly has an 'any' type.

    // No implicit-'any' errors.
    var m_f9 = () => "";

    // Implicit-'any' error for x.
    var m_f10 = (x) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // Implicit-'any' errors for x, y, and z.
    var m_f11 = (x, y, z) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x and z.
    var m_f12 = (x, y: any, z) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' errors for r.
    var m_f13 = (...r) => "";
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x and r.
    var m_f14 = (x, ...r) => "";
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
    //~| ERROR: Parameter 'x' implicitly has an 'any' type.
}