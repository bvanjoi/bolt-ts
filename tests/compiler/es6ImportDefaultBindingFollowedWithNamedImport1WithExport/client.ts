export import defaultBinding1, { } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x1: number = defaultBinding1;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding2, { a } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
//~| ERROR: Module '"./server"' has no exported member 'a'. Did you mean to use 'import a from "./server"' instead?
export var x1: number = defaultBinding2;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding3, { a as b } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
//~| ERROR: Module '"./server"' has no exported member 'a'. Did you mean to use 'import a from "./server"' instead?
export var x1: number = defaultBinding3;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding4, { x, a as y } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
//~| ERROR: Module '"./server"' has no exported member 'a'. Did you mean to use 'import a from "./server"' instead?
//~| ERROR: Module '"./server"' has no exported member 'x'. Did you mean to use 'import x from "./server"' instead?
export var x1: number = defaultBinding4;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding5, { x as z,  } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
//~| ERROR: Module '"./server"' has no exported member 'x'. Did you mean to use 'import x from "./server"' instead?
export var x1: number = defaultBinding5;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
export import defaultBinding6, { m,  } from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
//~| ERROR: Module '"./server"' has no exported member 'm'. Did you mean to use 'import m from "./server"' instead?
export var x1: number = defaultBinding6;
//~^ ERROR: Cannot redeclare exported variable 'x1'.
