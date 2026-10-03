// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorAccidentalCallDiagnostic.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// https://github.com/microsoft/TypeScript/issues/24554
class Test24554 {
    get property(): number { return 1; }
}
function test24554(x: Test24554) {
    return x.property();
    //~^ ERROR: This expression is not callable.
}
