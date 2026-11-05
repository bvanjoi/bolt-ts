import { Member } from './b';
//~^ ERROR: 'Member' is declared but its value is never read.
import d, { Member as M } from './b';
//~^ ERROR: All imports in import declaration are unused.
import * as ns from './b';
//~^ ERROR: 'ns' is declared but its value is never read.
import r = require("./b");
//~^ ERROR: 'r' is declared but its value is never read.
