import {Enum} from "./enum";

import EnumA = Enum.A;
//~^ ERROR: Import declaration conflicts with local declaration of 'EnumA'.
//~| ERROR: Individual declarations in merged declaration 'EnumA' must be all exported or all local.
export type EnumA = [string] | [string, number];
//~^ ERROR: Individual declarations in merged declaration 'EnumA' must be all exported or all local.
