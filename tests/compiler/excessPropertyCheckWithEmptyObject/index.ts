// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/excessPropertyCheckWithEmptyObject.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Excess property error expected here
Object.defineProperty(window, "prop", { value: "v1.0.0", readonly: false });
//~^ ERROR: Object literal may only specify known properties, and 'readonly' does not exist in type 'PropertyDescriptor & ThisType<any>'.

interface A { x?: string }

// Excess property error expected here
let a: A & ThisType<any> = { y: 10 };
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type 'A & ThisType<any>'.

interface Empty {}

// Excess property error expected here
let x: Empty & { x: number } = { y: "hello" };
//~^ ERROR: Object literal may only specify known properties, and 'y' does not exist in type 'Empty & { x: number; }'.
