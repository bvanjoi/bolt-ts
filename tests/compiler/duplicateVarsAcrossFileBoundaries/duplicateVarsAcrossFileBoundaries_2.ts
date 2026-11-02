var x = "";
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'x' must be of type 'number', but here has type 'string'.
var y = 3;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'y' must be of type 'string', but here has type 'number'.
var z = false;
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'z' must be of type 'number', but here has type 'boolean'.
