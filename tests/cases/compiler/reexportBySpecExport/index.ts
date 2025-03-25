import { CONST_A } from './b';

const a: string = CONST_A;
//~^ ERROR: Type 'number' is not assignable to type 'string'.

import { NOT_EXIST_1 } from './b';
//~^ ERROR: Module './b' has no exported member 'NOT_EXIST_1'.
import { NOT_EXIST_2 } from './c';
//~^ ERROR: Module './c' has no exported member 'NOT_EXIST_2'.

import { TYPE_A } from './b';

const typeA: TYPE_A = '42';
//~^ ERROR: Type 'string' is not assignable to type 'number'.

import type { TYPE_B } from './b';

const typeB: TYPE_B = '42';
//~^ ERROR: Type 'string' is not assignable to type 'number'.