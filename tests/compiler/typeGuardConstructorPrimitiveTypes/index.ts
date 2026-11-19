// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeGuardConstructorPrimitiveTypes.ts`, Apache-2.0 License

//@compiler-options: target=esnext

// Narrow a union of primitive types
let var1: string | number | boolean | any[] | symbol | bigint;
if (var1.constructor === String) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
  //~| ERROR: Variable 'var1' is used before being assigned.
    var1; // string
    const a: 42 = var1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
}
if (var1.constructor === Number) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
  //~| ERROR: Variable 'var1' is used before being assigned.
    var1; // number
    const a: 42 = var1;
    //~^ ERROR: Type 'number' is not assignable to type '42'.
}
if (var1.constructor === Boolean) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
  //~| ERROR: Variable 'var1' is used before being assigned.
    var1; // boolean
    const a: 42 = var1;
    //~^ ERROR: Type 'boolean' is not assignable to type '42'.
}
if (var1.constructor === Array) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
    var1; // any[]
    const a: 42 = var1;
    //~^ ERROR: Type 'any[]' is not assignable to type '42'.
}
if (var1.constructor === Symbol) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
  //~| ERROR: Variable 'var1' is used before being assigned.
    var1; // symbol
    const a: 42 = var1;
    //~^ ERROR: Type 'symbol' is not assignable to type '42'.
}
if (var1.constructor === BigInt) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
  //~| ERROR: Variable 'var1' is used before being assigned.
    var1; // bigint
    const a: 42 = var1;
    //~^ ERROR: Type 'bigint' is not assignable to type '42'.
}

// Narrow a union of primitive object types
let var2: String | Number | Boolean | Symbol | BigInt;
if (var2.constructor === String) {
  //~^ ERROR: Variable 'var2' is used before being assigned.
    var2; // String
    const a: 42 = var2;
    //~^ ERROR: Type 'String' is not assignable to type '42'.
}
if (var2.constructor === Number) {
  //~^ ERROR: Variable 'var2' is used before being assigned.
    var2; // Number
    const a: 42 = var2;
    //~^ ERROR: Type 'Number' is not assignable to type '42'.
}
if (var2.constructor === Boolean) {
  //~^ ERROR: Variable 'var2' is used before being assigned.
    var2; // Boolean
    const a: 42 = var2;
    //~^ ERROR: Type 'Boolean' is not assignable to type '42'.
}
if (var2.constructor === Symbol) {
  //~^ ERROR: Variable 'var2' is used before being assigned.
    var2; // Symbol
    const a: 42 = var2;
    //~^ ERROR: Type 'Symbol' is not assignable to type '42'.
}
if (var2.constructor === BigInt) {
  //~^ ERROR: Variable 'var2' is used before being assigned.
    var2; // BigInt
    const a: 42 = var2;
    //~^ ERROR: Type 'BigInt' is not assignable to type '42'.
}
