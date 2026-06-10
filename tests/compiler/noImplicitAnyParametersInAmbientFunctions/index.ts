// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyParametersInAmbientFunctions.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

// No implicit-'any' errors.
declare function d_f1(): void;

// Implicit-'any' errors for x.
declare function d_f2(x): void;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.

// No implicit-'any' errors.
declare function d_f3(x: any): void;

// Implicit-'any' errors for x, y, and z.
declare function d_f4(x, y, z): void;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
//~| ERROR: Parameter 'y' implicitly has an 'any' type.
//~| ERROR: Parameter 'z' implicitly has an 'any' type.

// Implicit-'any' errors for x, and z.
declare function d_f5(x, y: any, z): void;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
//~| ERROR: Parameter 'z' implicitly has an 'any' type.

// Implicit-'any[]' errors for r.
declare function d_f6(...r): void;
//~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

// Implicit-'any'/'any[]' errors for x, r.
declare function d_f7(x, ...r): void;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
//~| ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

// Implicit-'any' errors for x1, y2, x3, and y3.
declare function d_f8(x1, y1: number): any;
//~^ ERROR: Parameter 'x1' implicitly has an 'any' type.
declare function d_f8(x2: string, y2): any;
//~^ ERROR: Parameter 'y2' implicitly has an 'any' type.
declare function d_f8(x3, y3): any;
//~^ ERROR: Parameter 'x3' implicitly has an 'any' type.
//~| ERROR: Parameter 'y3' implicitly has an 'any' type.

// No implicit-'any' errors.
declare var d_f9: () => string;

// Implicit-'any' error for x.
declare var d_f10: (x) => string;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.

// Implicit-'any' errors for x, y, and z.
declare var d_f11: (x, y, z) => string;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
//~| ERROR: Parameter 'y' implicitly has an 'any' type.
//~| ERROR: Parameter 'z' implicitly has an 'any' type.

// Implicit-'any' errors for x and z.
declare var d_f12: (x, y: any, z) => string;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
//~| ERROR: Parameter 'z' implicitly has an 'any' type.

// Implicit-'any[]' error for r.
declare var d_f13: (...r) => string;
//~^ ERROR: Rest parameter 'r' implicitly has an 'any[]' type.

// Implicit-'any'/'any[]' errors for x, r.
declare var d_f14: (x, ...r) => string;
//~^ ERROR: Parameter 'x' implicitly has an 'any' type.
//~| ERROR: Rest parameter 'r' implicitly has an 'any[]' type.
