// Should error at semicolon.
var f = () => ;
//~^ ERROR: Expression expected.
var b = 1 * 2 * 3 * 4;
var square = (x: number) => x * x;