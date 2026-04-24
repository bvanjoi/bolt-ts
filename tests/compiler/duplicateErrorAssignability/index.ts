// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateErrorAssignability.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface A {
    x: number;
}
interface B {
    y: string;
}

declare let b: B;
declare let a: A;
const x = a = b;
//~^ ERROR: Property 'x' is missing.
//~| ERROR: Property 'x' is missing.
let obj: { 3: string } = { 3: "three" };
obj[x];
//~^ ERROR: Type 'B' cannot be used as an index type.
