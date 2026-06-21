// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deeplyNestedAssignabilityErrorsCombined.ts`, Apache-2.0 License

//@compiler-options: target=es2015

let x = { a: { b: { c: { d: { e: { f() { return { g: "hello" }; } } } } } } };
let y = { a: { b: { c: { d: { e: { f() { return { g: 12345 }; } } } } } } };
x = y;
//~^ ERROR: Type '{ a: { b: { c: { d: { e: { f: () => { g: number; }; }; }; }; }; }; }' is not assignable to type '{ a: { b: { c: { d: { e: { f: () => { g: string; }; }; }; }; }; }; }'

class Ctor1 {
    g = "ok"
}

class Ctor2 {
    g = 12;
}

let x2 = { a: { b: { c: { d: { e: { f: Ctor1 } } } } } };
let y2 = { a: { b: { c: { d: { e: { f: Ctor2 } } } } } };
x2 = y2;
//~^ ERROR: Type '{ a: { b: { c: { d: { e: { f: typeof Ctor2; }; }; }; }; }; }' is not assignable to type '{ a: { b: { c: { d: { e: { f: typeof Ctor1; }; }; }; }; }; }'.
