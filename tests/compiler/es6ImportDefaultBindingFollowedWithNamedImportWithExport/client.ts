export import defaultBinding1, { } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export import defaultBinding2, { a } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x1: number = a;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding3, { a as b } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x1: number = b;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding4, { x, a as y } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x1: number = x;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export var x1: number = y;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding5, { x as z,  } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x1: number = z;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding6, { m,  } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x1: number = m;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export var {x1} = {x1: 10};
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export var {x2: x1} = {x2: 10};
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export var [x1] = [10];
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export var [{x1}] = [{x1: 10}];
//~^ ERROR: Cannot redeclare exported variable 'x1'.
