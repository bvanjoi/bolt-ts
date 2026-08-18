// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyParametersInInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

interface I {
    // Implicit-'any' errors for first two call signatures, x1, x2, z2.
    ();
    //~^ ERROR: Call signature, which lacks return-type annotation, implicitly has an 'any' return type.
    (x1);
    //~^ ERROR: Call signature, which lacks return-type annotation, implicitly has an 'any' return type.
    //~| ERROR: Parameter 'x1' implicitly has an 'any' type.
    (x2, y2: string, z2): any;
    //~^ ERROR: Parameter 'x2' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z2' implicitly has an 'any' type.

    // No implicit-'any' errors.
    f1(): void;

    // Implicit-'any' errors for x.
    f2(x): void;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // No implicit-'any' errors.
    f3(x: any): void;

    // Implicit-'any' errors for x, y, and z.
    f4(x, y, z): void;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x, and z.
    f5(x, y: any, z): void;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' errors for r.
    f6(...r): void;
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x, r.
    f7(x, ...r): void;
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
    //~| ERROR: Parameter 'x' implicitly has an 'any' type.


    // Implicit-'any' errors for x1, y2, x3, and y3.
    f8(x1, y1: number): any;
    //~^ ERROR: Parameter 'x1' implicitly has an 'any' type.
    f8(x2: string, y2): any;
    //~^ ERROR: Parameter 'y2' implicitly has an 'any' type.
    f8(x3, y3): any;
    //~^ ERROR: Parameter 'x3' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y3' implicitly has an 'any' type.

    // No implicit-'any' errors.
    f9: () => string;

    // Implicit-'any' errors for x.
    f10: (x) => string;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.

    // Implicit-'any' errors for x, y, and z.
    f11: (x, y, z) => string;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'y' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any' errors for x and z.
    f12: (x, y: any, z) => string;
    //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    //~| ERROR: Parameter 'z' implicitly has an 'any' type.

    // Implicit-'any[]' error for r.
    f13: (...r) => string;
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

    // Implicit-'any'/'any[]' errors for x, r.
    f14: (x, ...r) => string;
    //~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
    //~| ERROR: Parameter 'x' implicitly has an 'any' type.
}