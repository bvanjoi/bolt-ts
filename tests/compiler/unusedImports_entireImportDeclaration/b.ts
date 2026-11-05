import d1, { a as a1, b as b1 } from "./a";
//~^ ERROR: All imports in import declaration are unused.
import d2, * as ns from "./a";
//~^ ERROR: All imports in import declaration are unused.

import d3, { a as a2, b as b2 } from "./a";
//~^ ERROR: 'a2' is declared but its value is never read.
//~| ERROR: 'b2' is declared but its value is never read.
d3;
import d4, * as ns2 from "./a";
//~^ ERROR: 'ns2' is declared but its value is never read.
d4;
import d5, * as ns3 from "./a";
//~^ ERROR: 'd5' is declared but its value is never read.
ns3;