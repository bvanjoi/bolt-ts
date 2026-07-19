// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/spreadIntersection.ts`, Apache-2.0 License

var intersection: { a: number } & { b: string };

var o1: { a: number, b: string };
var o1 = { ...intersection };
//~^ ERROR: Variable 'intersection' is used before being assigned.

var o2: { a: number, b: string, c: boolean };
var o2 = { ...intersection, c: false };
//~^ ERROR: Variable 'intersection' is used before being assigned.

var o3: {a: number, b: number } = { ...intersection };
//~^ ERROR: Type '{ a: number; b: string; }' is not assignable to type '{ a: number; b: number; }'.
//~| ERROR: Variable 'intersection' is used before being assigned.

var o4: { a:number } = { ...intersection };
//~^ ERROR: Variable 'intersection' is used before being assigned.
var o5: { b:string } = { ...intersection };
//~^ ERROR: Variable 'intersection' is used before being assigned.
var o6: { a:number, b:string } = { ...intersection };
//~^ ERROR: Variable 'intersection' is used before being assigned.
