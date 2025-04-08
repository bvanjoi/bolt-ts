// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/spreadIntersection.ts`, Apache-2.0 License

var intersection: { a: number } & { b: string };

var o1: { a: number, b: string };
var o1 = { ...intersection };

var o2: { a: number, b: string, c: boolean };
var o2 = { ...intersection, c: false };

var o3: {a: number, b: number } = { ...intersection };
//~^ ERROR: Type '{ b: string; a: number; }' is not assignable to type '{ a: number; b: number; }'.

var o4: { a:number } = { ...intersection };
var o5: { b:string } = { ...intersection };
var o6: { a:number, b:string } = { ...intersection };