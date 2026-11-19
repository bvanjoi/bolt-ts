import defaultBinding, * as nameSpaceBinding  from "./server";
//~^ ERROR: Module './server' has no default export.
export var x = new nameSpaceBinding.a();