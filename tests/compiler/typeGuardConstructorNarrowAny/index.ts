// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeGuardConstructorNarrowAny.ts`, Apache-2.0 License

//@compiler-options: target=esnext

// Narrowing any to primitives
let var1: any;

if (var1.constructor === String) {
    var1; // String
    let a: 42 = var1;
    //~^ ERROR: Type 'String' is not assignable to type '42'.
}
if (var1.constructor === Number) {
    var1; // Number
    let a: 42 = var1;
    //~^ ERROR: Type 'Number' is not assignable to type '42'.
}
if (var1.constructor === Boolean) {
    var1; // Boolean
    let a: 42 = var1;
    //~^ ERROR: Type 'Boolean' is not assignable to type '42'.
}
if (var1.constructor === Array) {
    var1; // any[]
    let a: 42 = var1;
    //~^ ERROR: Type 'any[]' is not assignable to type '42'.
}
if (var1.constructor === Symbol) {
    var1; // Symbol
    let a: 42 = var1;
    //~^ ERROR: Type 'Symbol' is not assignable to type '42'.
}
if (var1.constructor === BigInt) {
    var1; // BigInt
    let a: 42 = var1;
    //~^ ERROR: Type 'BigInt' is not assignable to type '42'.
}
