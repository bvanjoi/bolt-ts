// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParamAssignmentCompat.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface I1 {
    (p1: number, p2: string): void;
}
interface I2 {
    p1: I1;
    m1(p1?: string): I1;
}
declare var i2: I2;
var c: I1 = i2.p1; // should be ok
var d: I1 = i2.m1; // should error
//~^ ERROR: Type '(p1: undefined | string) => I1' is not assignable to type 'I1'.
