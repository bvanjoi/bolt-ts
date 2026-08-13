// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeGuardConstructorNarrowPrimitivesInUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Union of primitives, number, arrays, and C1
let var1: number | "hello" | "world" | true | false | number[] | string[];

if (var1.constructor === Number) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
    var1; // number
    const a: 42 = var1;
    //~^ ERROR: Type 'number' is not assignable to type '42'.
}

if (var1.constructor === String) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
    var1; // "hello" | "world"
    const a: 42 = var1;
    //~^ ERROR: Type 'string' is not assignable to type '42'.
    const b: "hello" | "world" = var1;
}

if (var1.constructor === Boolean) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
    var1; // boolean
    const a: 42 = var1;
    //~^ ERROR: Type 'boolean' is not assignable to type '42'.
}

if (var1.constructor === Array) {
  //~^ ERROR: Variable 'var1' is used before being assigned.
    var1; // number[] | string[]
    const a: 42 = var1;
    //~^ ERROR: Type 'number[] | string[]' is not assignable to type '42'.
}