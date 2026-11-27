export import defaultBinding, * as nameSpaceBinding  from "./server";
//~^ ERROR: Module './server' has no default export.
//~| ERROR: An import declaration cannot have modifiers.
export var x: number = nameSpaceBinding.a;