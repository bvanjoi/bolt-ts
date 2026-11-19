import defaultBinding1, { } from "./server";
export var x1 = new defaultBinding1();
import defaultBinding2, { a } from "./server";
//~^ ERROR: Module '"./server"' has no exported member 'a'. Did you mean to use 'import a from "./server"' instead?
export var x2 = new defaultBinding2();
import defaultBinding3, { a as b } from "./server";
//~^ ERROR: Module '"./server"' has no exported member 'a'. Did you mean to use 'import a from "./server"' instead?
export var x3 = new defaultBinding3();
import defaultBinding4, { x, a as y } from "./server";
//~^ ERROR: Module '"./server"' has no exported member 'x'. Did you mean to use 'import x from "./server"' instead?
//~| ERROR: Module '"./server"' has no exported member 'a'. Did you mean to use 'import a from "./server"' instead?
export var x4 = new defaultBinding4();
import defaultBinding5, { x as z,  } from "./server";
//~^ ERROR: Module '"./server"' has no exported member 'x'. Did you mean to use 'import x from "./server"' instead?
export var x5 = new defaultBinding5();
import defaultBinding6, { m,  } from "./server";
//~^ ERROR: Module '"./server"' has no exported member 'm'. Did you mean to use 'import m from "./server"' instead?
export var x6 = new defaultBinding6();