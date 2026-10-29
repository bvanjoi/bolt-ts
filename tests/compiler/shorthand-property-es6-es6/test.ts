import {foo} from './foo';
//~^ ERROR: Cannot find module './foo' or its corresponding type declarations.
const baz = 42;
const bar = { foo, baz };
