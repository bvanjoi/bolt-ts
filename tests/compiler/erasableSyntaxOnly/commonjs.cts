import foo = require("./other.cjs");
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
export = foo;
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
