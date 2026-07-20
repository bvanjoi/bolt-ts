// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericTypeWithMultipleBases1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export interface I1 {
    m1: () => void;
}
 
export interface I2 {
    m2: () => void;
}
 
export interface I3<T> extends I1, I2 {
//export interface I3<T> extends I2, I1 {
    p1: T;
}
 
var x: I3<number>;
x.p1;
//~^ ERROR: Variable 'x' is used before being assigned.
x.m1();
//~^ ERROR: Variable 'x' is used before being assigned.
x.m2();
//~^ ERROR: Variable 'x' is used before being assigned.

