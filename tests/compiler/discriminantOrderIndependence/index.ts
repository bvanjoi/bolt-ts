// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/discriminantOrderIndependence.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface A {
    subType: "b";
    type: "a";
}

declare let order1:
    | { type: "1" }
    | A
    | { type: "2" }
    | { type: "3" }
    | undefined;

// Should NOT error: 'order1' is possibly 'undefined' after the guard
if (order1 && order1.type === "a") {
    order1.type; // Should be OK
    let b: '42' = order1.type;
    //~^ ERROR: Type '"a"' is not assignable to type '"42"'.
}

interface B {
    subType: "b";
    type: "a";
}

declare let order2:
    | { type: "1" }
    | { type: "2" }
    | { type: "3" }
    | B
    | undefined;

// Should NOT error: 'order2' is possibly 'undefined' after the guard
if (order2 && order2.type === "a") {
    order2.type; // Should be OK
    let b: '42' = order2.type;
    //~^ ERROR: Type '"a"' is not assignable to type '"42"'.
}

// Also test with !. type assertion
if (order1 && order1.type === "a") {
    order1.type; // Should be OK
    let b: '42' = order1.type;
    //~^ ERROR: Type '"a"' is not assignable to type '"42"'.
}
if (order2 && order2.type === "a") {
    order2.type; // Should be OK
    let b: '42' = order2.type;
    //~^ ERROR: Type '"a"' is not assignable to type '"42"'.
}