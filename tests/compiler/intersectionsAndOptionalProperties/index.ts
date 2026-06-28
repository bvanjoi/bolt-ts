// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/intersectionsAndOptionalProperties.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare let x: { a?: number, b: string };
declare let y: { a: null, b: string };
declare let z: { a: null } & { b: string };

x = y;  // Error
//~^ ERROR: Type '{ a: null; b: string; }' is not assignable to type '{ a: undefined | number; b: string; }'.
x = z;  // Error
//~^ ERROR: Type '{ a: null; } & { b: string; }' is not assignable to type '{ a: undefined | number; b: string; }'.

// Repro from #36604

interface To {
    field?: number;
    anotherField: string;
}

type From =  { field: null } & Omit<To, 'field'>;

function foo(v: From) {
    let x: To;
    x = v;  // Error
    //~^ ERROR: Type 'From' is not assignable to type 'To'.
    x.field = v.field; // Error
    //~^ ERROR: Type 'null' is not assignable to type 'undefined | number'.
}

// Repro from #38348

const yy: number[] & [number, ...number[]] = [1];
const xx: [number, ...number[]] = yy;
