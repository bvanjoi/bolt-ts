// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restIntersection.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var intersection: { x: number, y: number } & { w: string, z: string };

var rest1: { y: number, w: string, z: string };
var {x, ...rest1 } = intersection;
//~^ ERROR: Variable 'intersection' is used before being assigned.
//~| ERROR: Variable 'intersection' is used before being assigned.
//~| ERROR: Variable 'intersection' is used before being assigned.
