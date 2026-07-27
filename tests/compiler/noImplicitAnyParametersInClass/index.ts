// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyParametersInClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

class C {
    // No implicit-'any' errors.
    public pub_f1(): void { }

    // Implicit-'any' errors for x.
    public pub_f2(x): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // No implicit-'any' errors.
    public pub_f3(x: any): void { }

    // Implicit-'any' errors for x, y, and z.
    public pub_f4(x, y, z): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x, and z.
    public pub_f5(x, y: any, z): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' errors for r.
    public pub_f6(...r): void { }
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x, r.
    public pub_f7(x, ...r): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any' errors for x1, y2, x3, and y3.
    public pub_f8(x1, y1: number): any;
    //~^ ERROR: Parameter 'x1' implicitly has an 'any' type.
    public pub_f8(x2: string, y2): any;
    //~^ ERROR: Parameter 'y2' implicitly has an 'any' type.
    public pub_f8(x3, y3): any { }
    //~^ ERROR: Parameter 'x3' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y3' implicitly has an 'any' type.

    // No implicit-'any' errors.
    public pub_f9 = () => "";

    // Implicit-'any' errors for x.
    public pub_f10 = (x) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // Implicit-'any' errors for x, y, and z.
    public pub_f11 = (x, y, z) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x and z.
    public pub_f12 = (x, y: any, z) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' error for r.
    public pub_f13 = (...r) => "";
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x, r.
    public pub_f14 = (x, ...r) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    ///////////////////////////////////////////

    // No implicit-'any' errors.
    private priv_f1(): void { }

    // Implicit-'any' errors for x.
    private priv_f2(x): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // No implicit-'any' errors.
    private priv_f3(x: any): void { }

    // Implicit-'any' errors for x, y, and z.
    private priv_f4(x, y, z): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x, and z.
    private priv_f5(x, y: any, z): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' errors for r.
    private priv_f6(...r): void { }
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x, r.
    private priv_f7(x, ...r): void { }
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any' errors for x1, y2, x3, and y3.
    private priv_f8(x1, y1: number): any;
    //~^ ERROR: Parameter 'x1' implicitly has an 'any' type.
    private priv_f8(x2: string, y2): any;
    //~^ ERROR: Parameter 'y2' implicitly has an 'any' type.
    private priv_f8(x3, y3): any { }
    //~^ ERROR: Parameter 'x3' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y3' implicitly has an 'any' type.

    // No implicit-'any' errors.
    private priv_f9 = () => "";

    // Implicit-'any' errors for x.
    private priv_f10 = (x) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // Implicit-'any' errors for x, y, and z.
    private priv_f11 = (x, y, z) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x and z.
    private priv_f12 = (x, y: any, z) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' error for r.
    private priv_f13 = (...r) => "";
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x, r.
    private priv_f14 = (x, ...r) => "";
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
}