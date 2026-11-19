// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeWithMultipleBases2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

export interface I1 {
    m1: () => void;
}
 
export interface I2 {
    m2: () => void;
}
 
export interface I3<T> extends I2, I1 {
    p1: T;
}
 
var x: I3<number>;
x.p1;
//~^ ERROR: Variable 'x' is used before being assigned.
x.m1();
//~^ ERROR: Variable 'x' is used before being assigned.
x.m2();
//~^ ERROR: Variable 'x' is used before being assigned.

