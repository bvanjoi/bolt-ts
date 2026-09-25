// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyDeclareFunctionWithoutFormalType2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
//@compiler-options: declaration

// generates function fn1(): number;
function fn1() {
    var x: number;
    return x;
    //~^ ERROR: Variable 'x' is used before being assigned.
}
// generates function fn2(): any;
function fn2(): any {
    var x: any;
    return x;
}
// generates function fn3();
function fn3() {
    var x: any;
    return x;
}
