export import defaultBinding, * as nameSpaceBinding  from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x: number = defaultBinding;