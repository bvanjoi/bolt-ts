// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/bestCommonTypeWithOptionalProperties.ts`, Apache-2.0 License

interface X { foo: string }
interface Y extends X { bar?: number }
interface Z extends X { bar: string }

var x: X;
var y: Y;
var z: Z;

// All these arrays should be X[]
var b1 = [x, y, z];
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
//~| ERROR: Variable 'z' is used before being assigned.
var b2 = [x, z, y];
//~^ ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'z' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
var b3 = [y, x, z];
//~^ ERROR: Variable 'y' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'z' is used before being assigned.
var b4 = [y, z, x];
//~^ ERROR: Variable 'y' is used before being assigned.
//~| ERROR: Variable 'z' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
var b5 = [z, x, y];
//~^ ERROR: Variable 'z' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
var b6 = [z, y, x];
//~^ ERROR: Variable 'z' is used before being assigned.
//~| ERROR: Variable 'y' is used before being assigned.
//~| ERROR: Variable 'x' is used before being assigned.

var a1: X[] = b1;
var a2: X[] = b2;
var a3: X[] = b3;
var a4: X[] = b4;
var a5: X[] = b5;
var a6: X[] = b6;
