import defaultBinding1, { } from "./server";
//~^ ERROR: Module './server' has no default export.
import defaultBinding2, { a } from "./server";
//~^ ERROR: Module './server' has no default export.
export var x1 = new a();
import defaultBinding3, { a11 as b } from "./server";
//~^ ERROR: Module './server' has no default export.
export var x2 = new b();
import defaultBinding4, { x, a12 as y } from "./server";
//~^ ERROR: Module './server' has no default export.
export var x4 = new x();
export var x5 = new y();
import defaultBinding5, { x11 as z,  } from "./server";
//~^ ERROR: Module './server' has no default export.
export var x3 = new z();
import defaultBinding6, { m,  } from "./server";
//~^ ERROR: Module './server' has no default export.
export var x6 = new m();
