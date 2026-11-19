export import defaultBinding from "./server";
//~^ ERROR: An import declaration cannot have modifiers.
export var x = defaultBinding;
export import defaultBinding2 from "./server"; // non referenced
//~^ ERROR: An import declaration cannot have modifiers.
