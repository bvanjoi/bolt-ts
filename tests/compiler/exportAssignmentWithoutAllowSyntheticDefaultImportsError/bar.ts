export = bar;
//~^ ERROR: Export assignment cannot be used when targeting ECMAScript modules. Consider using 'export default' or another module format instead.
function bar() {}